
module Main where

import           Him.EscapeCodes
import           Him.Terminal

import           Control.Exception (throw)
import           Control.Monad (void, forM_)
import           Data.Char (isPrint)
import           Data.Maybe (fromMaybe)
import           System.Exit
import           System.IO (hFlush, stdout)
import           System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import           System.Posix.Signals (raiseSignal, sigTSTP)

import           Yi.Rope (YiString)
import qualified Yi.Rope as Rope

data Cursor = Cursor !Int !Int
  deriving (Eq, Show)

blankCursor :: Cursor
blankCursor = Cursor 0 0

cursorUp, cursorDown, cursorLeft, cursorRight, cursorCol, cursorRow :: Int -> Cursor -> Cursor
cursorUp    n (Cursor row col) = Cursor (row - n) col
cursorDown  n (Cursor row col) = Cursor (row + n) col
cursorLeft  n (Cursor row col) = Cursor row (col - n)
cursorRight n (Cursor row col) = Cursor row (col + n)
cursorCol   n (Cursor row _)   = Cursor row n
cursorRow   n (Cursor _ col)   = Cursor n col

startOfLine = cursorCol 0
endOfLine buf cur@(Cursor row col) = cursorCol (lineLength buf row col + 1) cur

data Buffer = Buffer ![YiString]
  deriving (Eq, Show)

emptyBuffer :: Buffer
emptyBuffer = Buffer [Rope.empty]

lineCount :: Buffer -> Int
lineCount (Buffer lines) = length lines

lineLength :: Buffer -> Int -> Int -> Int
lineLength (Buffer lines) row col = Rope.length (lines !! row)

getLineAt :: Int -> Buffer -> YiString
getLineAt n (Buffer lines) = lines !! n

replaceLine :: Int -> Buffer -> [YiString] -> Buffer
replaceLine n (Buffer lines) newLines =
  Buffer (take n lines ++ newLines ++ drop (n + 1) lines)

deleteLine :: Int -> Buffer -> Buffer
deleteLine n buf = replaceLine n buf []

insertAt :: Int -> Int -> Buffer -> String -> Buffer
insertAt row col buf str =
  let line            = getLineAt row buf
      (before, after) = Rope.splitAt col line
      newLine         = Rope.concat [before, Rope.fromString str, after]
   in replaceLine row buf [newLine]

deleteAt :: Int -> Int -> Buffer -> Buffer
deleteAt row col buf =
  let line            = getLineAt row buf
      (before, after) = Rope.splitAt col line
   in
     if Rope.null before
        then mergeWithPrevLine after
        else deleteLastChar before after
  where
    mergeWithPrevLine after =
      if row == 0
         then buf
         else
           let newLine = Rope.append (getLineAt (row - 1) buf) after
               newBuf = replaceLine (row - 1) buf [newLine]
            in deleteLine row newBuf

    deleteLastChar before after =
      let allButFirst = fromMaybe Rope.empty (Rope.init before)
          newLine = Rope.append allButFirst after
      in replaceLine row buf [newLine]

insertNewlineAt :: Int -> Int -> Buffer -> Buffer
insertNewlineAt row col buf =
  let line            = getLineAt row buf
      (before, after) = Rope.splitAt col line
   in replaceLine row buf [before, after]

data State = State !Buffer !Cursor
  deriving (Eq, Show)

blankState :: State
blankState = State emptyBuffer blankCursor

processBackspace :: Buffer -> Cursor -> State
processBackspace buf cur@(Cursor row col) =
  State (deleteAt row col buf) (cursorLeft 1 cur)

processNewline :: Buffer -> Cursor -> State
processNewline buf cur@(Cursor row col) =
  State (insertNewlineAt row col buf) (cursorDown 1 (cursorCol 0 cur))

processChar :: Buffer -> Cursor -> Char -> State
processChar buf cur@(Cursor row col) chr =
  State (insertAt row col buf [chr]) (cursorRight 1 cur)

processKeyStroke :: State -> String -> IO State
processKeyStroke state@(State buf cur) str = do
  -- throw (AppException str)
  case codeFromStr str of
    Just Abort           -> exitSuccess
    Just Halt            -> raiseSignal sigTSTP >> return state
    Just (CursorUp n)    -> return $ State buf (cursorUp n cur)
    Just (CursorDown n)  -> return $ State buf (cursorDown n cur)
    Just (CursorRight n) -> return $ State buf (cursorRight n cur)
    Just (CursorLeft n)  -> return $ State buf (cursorLeft n cur)
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

readInput :: IO String
readInput = fst <$> fdRead stdInput 3

render :: State -> IO ()
render (State (Buffer buf) (Cursor row col)) = do
  output (escapeCode Clear)
  output (escapeCode (MoveCursor 0 0))
  forM_ buf $ \line -> do
    output (Rope.toString line)
    output (escapeCode (CursorDown 1))
    output (escapeCode (CursorCol 0))
  output (escapeCode (MoveCursor row col))

clamp :: State -> State
clamp (State buf (Cursor row col)) =
  let clampedRow = min (max 0 row) (lineCount buf - 1)
      clampedCol = min (max 0 col) (lineLength buf clampedRow col)
   in State buf (Cursor clampedRow clampedCol)

loop :: State -> IO ()
loop state = do
  render state
  str      <- readInput
  newState <- processKeyStroke state str
  loop (clamp newState)

main :: IO ()
main = withRawInput (loop blankState)

