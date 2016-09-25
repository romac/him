module Main where

import System.Posix.Terminal (TerminalMode(..), TerminalAttributes(..), TerminalState(..), withoutMode, getTerminalAttributes, setTerminalAttributes)
import System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)

import System.IO (hFlush, stdout)
import System.Exit
import Data.Typeable (Typeable)
import Control.Exception (throw, finally, catch, Exception, IOException)
import Control.Monad (void, forM_)

import qualified System.Console.ANSI as ANSI

allTermModes :: [TerminalMode]
allTermModes =
  [ InterruptOnBreak
  , MapCRtoLF
  , IgnoreBreak
  , IgnoreCR
  , IgnoreParityErrors
  , MapLFtoCR
  , CheckParity
  , StripHighBit
  , StartStopInput
  , StartStopOutput
  , MarkParityErrors
  , ProcessOutput
  , LocalMode
  , ReadEnable
  , TwoStopBits
  , HangupOnClose
  , EnableParity
  , OddParity
  , EnableEcho
  , EchoErase
  , EchoKill
  , EchoLF
  , ProcessInput
  , ExtendedFunctions
  , KeyboardInterrupts
  , NoFlushOnInterrupt
  , BackgroundWriteInterrupt
  ]

rawTermSettings :: TerminalAttributes -> TerminalAttributes
rawTermSettings = foldr (.) id funs
  where funs = flip withoutMode <$> allTermModes

setTermAttrsNow settings = setTerminalAttributes stdInput settings Immediately

withRawInput :: IO a -> IO a
withRawInput application = do
  oldTermSettings <- getTerminalAttributes stdInput
  let newTermSettings = rawTermSettings oldTermSettings
  setTermAttrsNow newTermSettings
  application
    `finally` setTermAttrsNow oldTermSettings

data AppException = AppException !String
  deriving (Show, Typeable)

instance Exception AppException

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

data Buffer = Buffer ![String]
  deriving (Eq, Show)

emptyBuffer :: Buffer
emptyBuffer = Buffer [""]

lineCount :: Buffer -> Int
lineCount (Buffer lines) = length lines

lineLength :: Buffer -> Int -> Int -> Int
lineLength (Buffer lines) row col = length (lines !! row)

insertAt :: Buffer -> Cursor -> String -> Buffer
insertAt (Buffer lines) (Cursor row col) str =
  Buffer (init lines ++ [last lines ++ str])

data State = State !Buffer !Cursor
  deriving (Eq, Show)

blankState :: State
blankState = State emptyBuffer blankCursor

processBackspace :: Buffer -> Cursor -> State
processBackspace (Buffer lines) cur@(Cursor row col) =
  State (Buffer (init lines ++ [init (last lines)])) (cursorLeft 1 cur)

processNewline :: Buffer -> Cursor -> State
processNewline (Buffer lines) cur@(Cursor row col) =
  State (Buffer (lines ++ [""])) ((cursorCol 1 . cursorDown 1) cur)

processChar :: Buffer -> Cursor -> String -> State
processChar buf cur str =
  State (insertAt buf cur str) (cursorRight 1 cur)

processKeyStroke :: State -> String -> IO State
processKeyStroke (State buf cur) str = do
  -- throw (AppException str)
  case codeFromStr str of
    Just Abort           -> exitSuccess
    Just (CursorUp n)    -> return $ State buf (cursorUp n cur)
    Just (CursorDown n)  -> return $ State buf (cursorDown n cur)
    Just (CursorRight n) -> return $ State buf (cursorRight n cur)
    Just (CursorLeft n)  -> return $ State buf (cursorLeft n cur)
    Just StartOfLine     -> return $ State buf (startOfLine cur)
    Just EndOfLine       -> return $ State buf (endOfLine buf cur)
    Just Backspace       -> return $ processBackspace buf cur
    Just NewLine         -> return $ processNewline buf cur
    Nothing              -> return $ processChar buf cur str

data EscapeCode
  = Clear
  | MoveCursor !Int !Int
  | CursorUp !Int
  | CursorDown !Int
  | CursorRight !Int
  | CursorLeft !Int
  | CursorCol !Int
  | ClearLine
  | Backspace
  | StartOfLine
  | EndOfLine
  | NewLine
  | Abort
  deriving (Eq, Show)

escapeCode :: EscapeCode -> String
escapeCode Clear                = ANSI.clearScreenCode
escapeCode (MoveCursor row col) = ANSI.setCursorPositionCode row col
escapeCode (CursorUp rows)      = ANSI.cursorUpCode rows
escapeCode (CursorDown rows)    = ANSI.cursorDownCode rows
escapeCode (CursorRight cols)   = ANSI.cursorForwardCode cols
escapeCode (CursorLeft cols)    = ANSI.cursorBackwardCode cols
escapeCode (CursorCol col)      = ANSI.setCursorColumnCode col
escapeCode ClearLine            = ANSI.clearLineCode
escapeCode Backspace            = "\DEL"
escapeCode NewLine              = "\r"
escapeCode Abort                = "\ETX"

codeFromStr :: String -> Maybe EscapeCode
codeFromStr "\ESC[A" = Just (CursorUp 1)
codeFromStr "\ESC[B" = Just (CursorDown 1)
codeFromStr "\ESC[C" = Just (CursorRight 1)
codeFromStr "\ESC[D" = Just (CursorLeft 1)
codeFromStr "\ESC[G" = Just (CursorCol 1)
codeFromStr "\DEL"   = Just Backspace
codeFromStr "\SOH"   = Just StartOfLine
codeFromStr "\ENQ"   = Just EndOfLine
codeFromStr "\ETX"   = Just Abort
codeFromStr "\r"     = Just NewLine
codeFromStr _        = Nothing

output :: String -> IO ()
output = void . fdWrite stdOutput

readInput :: IO String
readInput = fst <$> fdRead stdInput 3

render :: State -> IO ()
render (State (Buffer buf) (Cursor row col)) = do
  output (escapeCode Clear)
  output (escapeCode (MoveCursor 0 0))
  forM_ buf $ \line -> do
    output line
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

