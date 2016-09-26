
module Him.Buffer where

import           Yi.Rope (YiString)
import qualified Yi.Rope as Rope

import           Data.Maybe (fromMaybe)

data Buffer = Buffer ![YiString]
  deriving (Eq, Show)

empty :: Buffer
empty = Buffer [Rope.empty]

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

