
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Him
  ( runHim
  ) where

import           Data.Text.Zipper (TextZipper)
import qualified Data.Text.Zipper as Z

import qualified System.Console.ANSI as ANSI
import           System.Console.ANSI (SGR(..))
import           System.Console.Terminal.Size (fdSize, Window(..))

import           Him.EscapeCode  (EscapeCode)
import qualified Him.EscapeCode  as EC

import           Him.Exception

import qualified Him.Terminal    as Terminal

import           Him.Buffer      (Buffer, BufferF(..))
import qualified Him.Buffer      as Buffer

import           Him.State      (State(..), StateF)
import qualified Him.State      as State

import           Data.Functor.Coproduct
import           Data.Bifunctor (second)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char (isPrint)
import           Data.Monoid ((<>))

import           Control.Exception (throw, finally)
import           Control.Monad (void, forM_)
import           Control.Monad.State.Strict (StateT, MonadState(..), evalStateT, modify', gets)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.FreeC
import           Control.Monad.IO.Class

import           System.Exit (exitSuccess)
import           System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import           System.Posix.Signals (raiseSignal, sigTSTP)

type HimM = StateT State IO

type HimF = BufferF :+: AppF

type Him = FreeC HimF

data AppF a where
  Exit    :: AppF ()
  Suspend :: AppF ()
  Output  :: Text -> AppF ()
  ReadInput :: AppF (Text, Int)
  WindowSize :: AppF (Maybe (Window Int))

exit :: AppF :<: f => FreeC f ()
exit = injectFreeC Exit

suspend :: AppF :<: f => FreeC f ()
suspend = injectFreeC Suspend

output :: AppF :<: f => Text -> FreeC f ()
output = injectFreeC . Output

output' :: AppF :<: f => String -> FreeC f ()
output' = injectFreeC . Output . T.pack

sendEscapeCode :: AppF :<: f => EscapeCode -> FreeC f ()
sendEscapeCode = output . T.pack . EC.toString

setSGRCode :: AppF :<: f => [SGR] -> FreeC f ()
setSGRCode = output' . ANSI.setSGRCode

readInput :: AppF :<: f => FreeC f (Text, Int)
readInput = injectFreeC ReadInput

windowSize :: AppF :<: f => FreeC f (Maybe (Window Int))
windowSize = injectFreeC WindowSize

processBackspace :: BufferF :<: f => FreeC f ()
processBackspace = Buffer.deletePrevChar

processNewline :: BufferF :<: f => FreeC f ()
processNewline = Buffer.breakLine

processChar :: BufferF :<: f => Char -> FreeC f ()
processChar chr = Buffer.insertChar chr

handleKey :: (AppF :<: f, BufferF :<: f) => Text -> Int -> FreeC f ()
handleKey key      1 | isPrint (head (T.unpack key)) = handlePrintableChar (T.head key)
handleKey ctrlCode _                                 = handleControlCode (T.unpack ctrlCode)

handleControlCode :: (AppF :<: f, BufferF :<: f) => String -> FreeC f ()
handleControlCode ctrlCode =
  case EC.fromString ctrlCode of
    Just EC.Abort           -> exit
    Just EC.Halt            -> suspend
    Just (EC.CursorUp n)    -> Buffer.moveUp
    Just (EC.CursorDown n)  -> Buffer.moveDown
    Just (EC.CursorRight n) -> Buffer.moveRight
    Just (EC.CursorLeft n)  -> Buffer.moveLeft
    Just EC.StartOfLine     -> Buffer.gotoBOL
    Just EC.EndOfLine       -> Buffer.gotoEOL
    Just EC.Backspace       -> processBackspace
    Just EC.NewLine         -> processNewline
    Nothing                 -> return ()

handlePrintableChar :: BufferF :<: f => Char -> FreeC f ()
handlePrintableChar chr = processChar chr

gutterWidth :: Int
gutterWidth = 6

gutter :: Text -> Text
gutter str = T.justifyRight gutterWidth ' ' str <> " "

lineNum :: Int -> Text
lineNum = gutter . T.pack . show

emptyLine :: Text
emptyLine = gutter "~"

outputLine :: AppF :<: f => Int -> Text -> FreeC f ()
outputLine num str = do
  setSGRCode [SetSwapForegroundBackground True]
  output $ lineNum num
  setSGRCode [Reset]
  sendEscapeCode (EC.MoveCursor (num - 1) (gutterWidth + 2))
  output str
  sendEscapeCode (EC.CursorDown 1)
  sendEscapeCode (EC.CursorCol 0)

outputEmptyLine :: AppF :<: f => FreeC f ()
outputEmptyLine = do
  setSGRCode [SetSwapForegroundBackground True]
  output emptyLine
  setSGRCode [Reset]
  sendEscapeCode (EC.CursorDown 1)
  sendEscapeCode (EC.CursorCol 0)

renderStatusBar :: (AppF :<: f, BufferF :<: f) => FreeC f ()
renderStatusBar = do
  setSGRCode [SetSwapForegroundBackground True]
  output " INSERT "
  fileName <- Buffer.getFileName
  case fileName of
    Just name -> output "|  " >> output name
    Nothing   -> return ()
  setSGRCode [Reset]

clearScreen :: AppF :<: f => FreeC f ()
clearScreen = do
  sendEscapeCode EC.Clear
  sendEscapeCode (EC.MoveCursor 0 0)

render :: (AppF :<: f, BufferF :<: f) => FreeC f ()
render = do
  Just (Window h w) <- windowSize
  clearScreen
  lines <- Buffer.getContents
  let withLinesNums = zip [1..] lines
  forM_ withLinesNums (uncurry outputLine)
  forM_ [length lines + 1 .. h - 2] (const outputEmptyLine)
  sendEscapeCode (EC.MoveCursor (h - 1) 0)
  renderStatusBar
  (row, col) <- Buffer.cursorPosition
  sendEscapeCode (EC.MoveCursor row (col + gutterWidth + 2))

loop :: (AppF :<: f, BufferF :<: f) => FreeC f ()
loop = do
  render
  (str, bytes) <- readInput
  newState     <- handleKey str bytes
  loop

withText' :: MonadState State m => (TextZipper Text -> TextZipper Text) -> m ()
withText' f = do
  state <- get
  let buf = State.buffer state
  let newBuf = buf { Buffer.contents = f (Buffer.contents buf)  }
  put $ state { buffer = newBuf }

withText :: MonadState State m => (TextZipper Text -> a) -> m a
withText f = do
  buf <- gets State.buffer
  return $ f (Buffer.contents buf)

runBuffer :: MonadState State m => BufferF ~> m
runBuffer GetFileName      = gets (Buffer.fileName . State.buffer)
runBuffer GetContents      = withText Z.getText
runBuffer CurrentLine      = withText Z.currentLine
runBuffer CursorPosition   = withText Z.cursorPosition
runBuffer LineLengths      = withText Z.lineLengths
runBuffer LineLimit        = withText Z.getLineLimit
runBuffer (MoveCursor pos) = withText' (Z.moveCursor pos)
runBuffer (InsertChar c)   = withText' (Z.insertChar c)
runBuffer (InsertMany cs)  = withText' (Z.insertMany cs)
runBuffer BreakLine        = withText' Z.breakLine
runBuffer GotoEOL          = withText' Z.gotoEOL
runBuffer GotoBOL          = withText' Z.gotoBOL
runBuffer DeletePrevChar   = withText' Z.deletePrevChar
runBuffer MoveRight        = withText' Z.moveRight
runBuffer MoveLeft         = withText' Z.moveLeft
runBuffer MoveUp           = withText' Z.moveUp
runBuffer MoveDown         = withText' Z.moveDown

runApp :: MonadIO m => AppF ~> m
runApp Exit       = liftIO exitSuccess
runApp Suspend    = liftIO (raiseSignal sigTSTP)
runApp WindowSize = liftIO (fdSize stdOutput)
runApp (Output t) = liftIO $ void (fdWrite stdOutput (T.unpack t))
runApp ReadInput  = liftIO $ do
  (t, n) <- fdRead stdInput 3
  return (T.pack t, fromIntegral n)

runHim' :: HimF ~> HimM
runHim' = runBuffer `coproduct` runApp

runHimIO :: Him ~> IO
runHimIO prog = evalStateT action State.mkBlankState
  where action = foldFreeC runHim' prog

runHim :: IO ()
runHim = Terminal.withRawInput $ do
  ANSI.setTitle "him"
  runHimIO loop
    -- `finally` ANSI.clearScreen

