
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Him.Buffer where

import           Data.Text        (Text)

import           Data.Text.Zipper (TextZipper)
import qualified Data.Text.Zipper as Z

import           Data.Functor.Coproduct
import           Control.Monad.FreeC

data Buffer = Buffer
  { fileName :: Maybe Text
  , contents :: TextZipper Text
  }
  deriving (Eq, Show)

mkEmptyBuffer :: Buffer
mkEmptyBuffer = Buffer Nothing (Z.textZipper [] Nothing)

data BufferF a where
  Empty          ::               BufferF Buffer
  GetFileName    ::               BufferF (Maybe Text)
  GetContents    ::               BufferF [Text]
  CurrentLine    ::               BufferF Text
  CursorPosition ::               BufferF (Int, Int)
  LineLengths    ::               BufferF [Int]
  LineLimit      ::               BufferF (Maybe Int)
  MoveCursor     :: (Int, Int) -> BufferF ()
  InsertChar     :: Char ->       BufferF ()
  InsertMany     :: Text ->       BufferF ()
  BreakLine      ::               BufferF ()
  KillToEOL      ::               BufferF ()
  KillToBOL      ::               BufferF ()
  GotoEOL        ::               BufferF ()
  GotoBOL        ::               BufferF ()
  DeletePrevChar ::               BufferF ()
  DeleteChar     ::               BufferF ()
  MoveRight      ::               BufferF ()
  MoveLeft       ::               BufferF ()
  MoveUp         ::               BufferF ()
  MoveDown       ::               BufferF ()
  TransposeChars ::               BufferF ()

empty :: BufferF :<: f => FreeC f Buffer
empty = injectFreeC Empty

getFileName :: BufferF :<: f => FreeC f (Maybe Text)
getFileName = injectFreeC GetFileName

getContents :: BufferF :<: f => FreeC f [Text]
getContents = injectFreeC GetContents

currentLine :: BufferF :<: f => FreeC f Text
currentLine = injectFreeC CurrentLine

cursorPosition :: BufferF :<: f => FreeC f (Int, Int)
cursorPosition = injectFreeC CursorPosition

lineLengths :: BufferF :<: f => FreeC f [Int]
lineLengths = injectFreeC LineLengths

lineLimit :: BufferF :<: f => FreeC f (Maybe Int)
lineLimit = injectFreeC LineLimit

moveCursor :: BufferF :<: f => (Int, Int) -> FreeC f ()
moveCursor = injectFreeC . MoveCursor

insertChar :: BufferF :<: f => Char -> FreeC f ()
insertChar = injectFreeC . InsertChar

insertMany :: BufferF :<: f => Text -> FreeC f ()
insertMany = injectFreeC . InsertMany

breakLine :: BufferF :<: f => FreeC f ()
breakLine = injectFreeC BreakLine

killToEOL :: BufferF :<: f => FreeC f ()
killToEOL = injectFreeC KillToEOL

killToBOL :: BufferF :<: f => FreeC f ()
killToBOL = injectFreeC KillToBOL

gotoEOL :: BufferF :<: f => FreeC f ()
gotoEOL = injectFreeC GotoEOL

gotoBOL :: BufferF :<: f => FreeC f ()
gotoBOL = injectFreeC GotoBOL

deletePrevChar :: BufferF :<: f => FreeC f ()
deletePrevChar = injectFreeC DeletePrevChar

deleteChar :: BufferF :<: f => FreeC f ()
deleteChar = injectFreeC DeleteChar

moveRight :: BufferF :<: f => FreeC f ()
moveRight = injectFreeC MoveRight

moveLeft :: BufferF :<: f => FreeC f ()
moveLeft = injectFreeC MoveLeft

moveUp :: BufferF :<: f => FreeC f ()
moveUp = injectFreeC MoveUp

moveDown :: BufferF :<: f => FreeC f ()
moveDown = injectFreeC MoveDown

transposeChars :: BufferF :<: f => FreeC f ()
transposeChars = injectFreeC TransposeChars

