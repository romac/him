
module Him
  ( runHim
  ) where

import System.Console.ANSI

import           Him.EscapeCode  (EscapeCode(..))
import qualified Him.EscapeCode  as EC

import           Him.Exception

import qualified Him.Terminal    as Terminal

import           Him.Buffer      (Buffer(..))
import qualified Him.Buffer      as Buffer

import           Him.Cursor      (Cursor(..))
import qualified Him.Cursor      as Cursor

import           Him.State      (State(..))
import qualified Him.State      as State

import           Control.Exception (throw)
import           Control.Monad (void, forM_)
import           Data.Bifunctor (second)
import           Data.Char (isPrint)
import           System.Exit (exitSuccess)
import           System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import           System.Posix.Signals (raiseSignal, sigTSTP)

import           Yi.Rope (YiString)
import qualified Yi.Rope as Rope

startOfLine :: Cursor -> Cursor
startOfLine = Cursor.col 0

endOfLine :: Buffer -> Cursor -> Cursor
endOfLine buf cur@(Cursor row col) = Cursor.col (Buffer.lineLength buf row col + 1) cur

processBackspace :: Buffer -> Cursor -> State
processBackspace buf cur@(Cursor row col) =
  State (Buffer.deleteAt row col buf) (Cursor.left 1 cur)

processNewline :: Buffer -> Cursor -> State
processNewline buf cur@(Cursor row col) =
  State (Buffer.insertNewlineAt row col buf) (Cursor.down 1 (Cursor.col 0 cur))

processChar :: Buffer -> Cursor -> Char -> State
processChar buf cur@(Cursor row col) chr =
  State (Buffer.insertAt row col buf [chr]) (Cursor.right 1 cur)

processKeyStroke :: State -> String -> Int -> IO State
processKeyStroke state@(State buf cur) str bytes = do
  -- throw $ AppException (show (str, bytes))
  case EC.fromString str of
    Just Abort           -> exitSuccess
    Just Halt            -> raiseSignal sigTSTP >> return state
    Just (CursorUp n)    -> return $ State buf (Cursor.up n cur)
    Just (CursorDown n)  -> return $ State buf (Cursor.down n cur)
    Just (CursorRight n) -> return $ State buf (Cursor.right n cur)
    Just (CursorLeft n)  -> return $ State buf (Cursor.left n cur)
    Just StartOfLine     -> return $ State buf (startOfLine cur)
    Just EndOfLine       -> return $ State buf (endOfLine buf cur)
    Just Backspace       -> return $ processBackspace buf cur
    Just NewLine         -> return $ processNewline buf cur
    Nothing              -> do
      let chr = head str
      if isPrint chr
         then return $ processChar buf cur chr
         else return state

output :: String -> IO ()
output = void . fdWrite stdOutput

sendEscapeCode :: EscapeCode -> IO ()
sendEscapeCode = output . EC.toString

readInput :: IO (String, Int)
readInput = second fromIntegral <$> fdRead stdInput 3

gutterWidth = 4

outputLine :: Int -> YiString -> IO ()
outputLine num str = do
  output $ setSGRCode [SetSwapForegroundBackground True]
  output $ " " ++ show num ++ " "
  output $ setSGRCode [Reset]
  sendEscapeCode (MoveCursor (num - 1) gutterWidth)
  output (Rope.toString str)
  sendEscapeCode (CursorDown 1)
  sendEscapeCode (CursorCol 0)

render :: State -> IO ()
render (State (Buffer buf) (Cursor row col)) = do
  sendEscapeCode Clear
  sendEscapeCode (MoveCursor 0 0)
  let withLinesNums = zip [1..] buf
  forM_ withLinesNums (uncurry outputLine)
  sendEscapeCode (MoveCursor row (col + gutterWidth))

clamp :: State -> State
clamp (State buf (Cursor row col)) =
  let clampedRow = min (max 0 row) (Buffer.lineCount buf - 1)
      clampedCol = min (max 0 col) (Buffer.lineLength buf clampedRow col)
   in State buf (Cursor clampedRow clampedCol)

loop :: State -> IO ()
loop state = do
  render state
  (str, bytes) <- readInput
  newState     <- processKeyStroke state str bytes
  loop (clamp newState)

runHim :: IO ()
runHim = Terminal.withRawInput (setTitle "him" >> loop State.blank)

