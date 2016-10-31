
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

import qualified Him.EscapeCode  as EC

import qualified Him.Terminal    as Terminal

import           Him.App         (AppF(..))
import qualified Him.App         as App

import           Him.Buffer      (BufferF(..))
import qualified Him.Buffer      as Buffer

import           Him.State       (State(..))
import qualified Him.State       as State

import           Data.Functor.Coproduct
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char (isPrint)
import           Data.Monoid ((<>))

import           Control.Exception (finally)
import           Control.Monad (void, forM_)
import           Control.Monad.State.Strict (StateT, MonadState(..), evalStateT, gets)
import           Control.Monad.FreeC
import           Control.Monad.IO.Class

import           System.Exit (exitSuccess)
import           System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import           System.Posix.Signals (raiseSignal, sigTSTP)

type HimM = StateT State IO

type HimF = BufferF :+: AppF

type Him = FreeC HimF

handleKey :: (AppF :<: f, BufferF :<: f) => Text -> Int -> FreeC f ()
handleKey key 1 | isPrint (head (T.unpack key)) =
  Buffer.insertChar (T.head key)

handleKey ctrlCode _ =
  handleControlCode (T.unpack ctrlCode)

handleControlCode :: (AppF :<: f, BufferF :<: f) => String -> FreeC f ()
handleControlCode ctrlCode =
  case EC.fromString ctrlCode of
    Just EC.Abort            -> App.exit
    Just EC.Halt             -> App.suspend
    Just (EC.CursorUp _)     -> Buffer.moveUp
    Just (EC.CursorDown _)   -> Buffer.moveDown
    Just (EC.CursorRight _)  -> Buffer.moveRight
    Just (EC.CursorLeft _)   -> Buffer.moveLeft
    Just (EC.MoveCursor x y) -> Buffer.moveCursor (x, y)
    Just EC.StartOfLine      -> Buffer.gotoBOL
    Just EC.EndOfLine        -> Buffer.gotoEOL
    Just EC.Backspace        -> Buffer.deletePrevChar
    Just EC.NewLine          -> Buffer.breakLine
    Just EC.Clear            -> return () -- TODO
    Just (EC.CursorCol _)    -> return () -- TODO
    Just EC.ClearLine        -> return () -- TODO
    Nothing                  -> return ()

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
  App.setSGRCode [SetSwapForegroundBackground True]
  App.output $ lineNum num
  App.setSGRCode [Reset]
  App.sendEscapeCode (EC.MoveCursor (num - 1) (gutterWidth + 2))
  App.output str
  App.sendEscapeCode (EC.CursorDown 1)
  App.sendEscapeCode (EC.CursorCol 0)

outputEmptyLine :: AppF :<: f => FreeC f ()
outputEmptyLine = do
  App.setSGRCode [SetSwapForegroundBackground True]
  App.output emptyLine
  App.setSGRCode [Reset]
  App.sendEscapeCode (EC.CursorDown 1)
  App.sendEscapeCode (EC.CursorCol 0)

renderStatusBar :: (AppF :<: f, BufferF :<: f) => FreeC f ()
renderStatusBar = do
  App.setSGRCode [SetSwapForegroundBackground True]
  App.output " INSERT "
  fileName <- Buffer.getFileName
  case fileName of
    Just name -> App.output "|  " >> App.output name
    Nothing   -> return ()
  App.setSGRCode [Reset]

clearScreen :: AppF :<: f => FreeC f ()
clearScreen = do
  App.sendEscapeCode EC.Clear
  App.sendEscapeCode (EC.MoveCursor 0 0)

render :: (AppF :<: f, BufferF :<: f) => FreeC f ()
render = do
  Just (Window h _) <- App.windowSize
  clearScreen
  contents <- Buffer.getContents
  let withLinesNums = zip [1..] contents
  forM_ withLinesNums (uncurry outputLine)
  forM_ [length contents + 1 .. h - 2] (const outputEmptyLine)
  App.sendEscapeCode (EC.MoveCursor (h - 1) 0)
  renderStatusBar
  (row, col) <- Buffer.cursorPosition
  App.sendEscapeCode (EC.MoveCursor row (col + gutterWidth + 2))

loop :: (AppF :<: f, BufferF :<: f) => FreeC f ()
loop = do
  render
  (str, bytes) <- App.readInput
  handleKey str bytes
  loop

withText' :: MonadState State m => (TextZipper Text -> TextZipper Text) -> m ()
withText' f = do
  st <- get
  let buf = State.buffer st
  let newBuf = buf { Buffer.contents = f (Buffer.contents buf)  }
  put $ st { buffer = newBuf }

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
runBuffer Empty            = undefined
runBuffer KillToEOL        = undefined
runBuffer KillToBOL        = undefined
runBuffer DeleteChar       = undefined
runBuffer TransposeChars   = undefined

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
    `finally` ANSI.clearScreen

