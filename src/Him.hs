
module Him
  ( runHim
  ) where

-- TODO: Remove
import          System.Console.ANSI hiding (clearScreen)
import          System.Console.Terminal.Size (fdSize, Window(..))

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

import           Control.Exception (throw, finally)
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

handleKey :: State -> String -> Int -> IO State
handleKey state [key]    1 | isPrint key = handlePrintableChar state key
handleKey state ctrlCode _               = handleControlCode state ctrlCode

handleControlCode :: State -> String -> IO State
handleControlCode state@(State buf cur) ctrlCode =
  case EC.fromString ctrlCode of
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
    Nothing              -> return state

handlePrintableChar :: State -> Char -> IO State
handlePrintableChar (State buf cur) chr = return $ processChar buf cur chr

output :: String -> IO ()
output = void . fdWrite stdOutput

sendEscapeCode :: EscapeCode -> IO ()
sendEscapeCode = output . EC.toString

readInput :: IO (String, Int)
readInput = second fromIntegral <$> fdRead stdInput 3

padLeftWith :: Int -> a -> [a] -> [a]
padLeftWith n x xs = replicate (n - length ys) x ++ ys
  where ys = take n xs

gutterWidth :: Int
gutterWidth = 6

lineNum :: Int -> String
lineNum num = padLeftWith gutterWidth ' ' (show num) ++ " "

outputLine :: Int -> YiString -> IO ()
outputLine num str = do
  output $ setSGRCode [SetSwapForegroundBackground True]
  output $ lineNum num
  output $ setSGRCode [Reset]
  sendEscapeCode (MoveCursor (num - 1) (gutterWidth + 2))
  output (Rope.toString str)
  sendEscapeCode (CursorDown 1)
  sendEscapeCode (CursorCol 0)

outputEmptyLine :: Int -> IO ()
outputEmptyLine num = do
  output $ setSGRCode [SetSwapForegroundBackground True]
  output $ lineNum num
  output $ setSGRCode [Reset]
  sendEscapeCode (CursorDown 1)
  sendEscapeCode (CursorCol 0)

renderStatusBar :: IO ()
renderStatusBar = do
  output $ setSGRCode [SetSwapForegroundBackground True]
  output " INSERT "
  output $ setSGRCode [Reset]

clearScreen :: IO ()
clearScreen = do
  sendEscapeCode Clear
  sendEscapeCode (MoveCursor 0 0)

render :: Window Int -> State -> IO ()
render (Window h w) (State (Buffer buf) (Cursor row col)) = do
  clearScreen
  let withLinesNums = zip [1..] buf
  forM_ withLinesNums (uncurry outputLine)
  forM_ [length buf + 1 .. h - 2] outputEmptyLine
  sendEscapeCode (MoveCursor (h - 1) 0)
  renderStatusBar
  sendEscapeCode (MoveCursor row (col + gutterWidth + 2))

clamp :: State -> State
clamp (State buf (Cursor row col)) =
  let clampedRow = min (max 0 row) (Buffer.lineCount buf - 1)
      clampedCol = min (max 0 col) (Buffer.lineLength buf clampedRow col)
   in State buf (Cursor clampedRow clampedCol)

loop :: State -> IO ()
loop state = do
  Just win <- fdSize stdOutput
  render win state
  (str, bytes) <- readInput
  newState     <- handleKey state str bytes
  loop (clamp newState)

runHim :: IO ()
runHim = Terminal.withRawInput $ do
  setTitle "him"
  loop State.blank
    `finally` clearScreen

