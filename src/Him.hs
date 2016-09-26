
module Him
  ( runHim
  ) where

import qualified Him.EscapeCode  as EC
import qualified Him.Terminal    as Terminal

import           Him.Buffer      (Buffer(..))
import qualified Him.Buffer      as Buffer

import           Him.Cursor      (Cursor(..))
import qualified Him.Cursor      as Cursor

import           Him.State      (State(..))
import qualified Him.State      as State

import           Control.Exception (throw)
import           Control.Monad (void, forM_)
import           Data.Char (isPrint)
import           System.Exit
import           System.IO (hFlush, stdout)
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

processKeyStroke :: State -> String -> IO State
processKeyStroke state@(State buf cur) str = do
  -- throw (AppException str)
  case EC.fromString str of
    Just EC.Abort           -> exitSuccess
    Just EC.Halt            -> raiseSignal sigTSTP >> return state
    Just (EC.CursorUp n)    -> return $ State buf (Cursor.up n cur)
    Just (EC.CursorDown n)  -> return $ State buf (Cursor.down n cur)
    Just (EC.CursorRight n) -> return $ State buf (Cursor.right n cur)
    Just (EC.CursorLeft n)  -> return $ State buf (Cursor.left n cur)
    Just EC.StartOfLine     -> return $ State buf (startOfLine cur)
    Just EC.EndOfLine       -> return $ State buf (endOfLine buf cur)
    Just EC.Backspace       -> return $ processBackspace buf cur
    Just EC.NewLine         -> return $ processNewline buf cur
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
  output (EC.toString EC.Clear)
  output (EC.toString (EC.MoveCursor 0 0))
  forM_ buf $ \line -> do
    output (Rope.toString line)
    output (EC.toString (EC.CursorDown 1))
    output (EC.toString (EC.CursorCol 0))
  output (EC.toString (EC.MoveCursor row col))

clamp :: State -> State
clamp (State buf (Cursor row col)) =
  let clampedRow = min (max 0 row) (Buffer.lineCount buf - 1)
      clampedCol = min (max 0 col) (Buffer.lineLength buf clampedRow col)
   in State buf (Cursor clampedRow clampedCol)

loop :: State -> IO ()
loop state = do
  render state
  str      <- readInput
  newState <- processKeyStroke state str
  loop (clamp newState)

runHim :: IO ()
runHim = Terminal.withRawInput (loop State.blank)

