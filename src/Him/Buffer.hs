
{-# LANGUAGE OverloadedStrings #-}

module Him.Buffer where

import           Data.Maybe (fromMaybe)

import qualified Data.Text        as T
import qualified Data.Text.Zipper as Z

data Buffer = Buffer !Z.TextZipper
  deriving (Eq, Show)

empty :: Buffer
empty = Buffer (Z.textZipper [T.empty] Nothing)

lineCount :: Buffer -> Int
lineCount (Buffer z) = T.length (Z.currentLine z)

lineLength :: Buffer -> Int -> Int -> Int
lineLength (Buffer z) row col = T.length (lines !! row)

getLineAt :: Int -> Buffer -> T.Text
getLineAt n (Buffer z) = lines !! n

replaceLine :: Int -> Buffer -> [T.Text] -> Buffer
replaceLine n (Buffer z) newLines =
  Buffer (take n lines ++ newLines ++ drop (n + 1) lines)

deleteLine :: Int -> Buffer -> Buffer
deleteLine n buf = replaceLine n buf []

insertAt :: Int -> Int -> Buffer -> String -> Buffer
insertAt row col buf str =
  let line            = getLineAt row buf
      (before, after) = Z.splitAt col line
      newLine         = Z.concat [before, Z.fromString str, after]
   in replaceLine row buf [newLine]

deleteAt :: Int -> Int -> Buffer -> Buffer
deleteAt row col buf =
  let line            = getLineAt row buf
      (before, after) = Z.splitAt col line
   in
     if Z.null before
        then mergeWithPrevLine after
        else deleteLastChar before after
  where
    mergeWithPrevLine after =
      if row == 0
         then buf
         else
           let newLine = Z.append (getLineAt (row - 1) buf) after
               newBuf = replaceLine (row - 1) buf [newLine]
            in deleteLine row newBuf

    deleteLastChar before after =
      let allButFirst = fromMaybe Z.empty (Z.init before)
          newLine = Z.append allButFirst after
      in replaceLine row buf [newLine]

insertNewlineAt :: Int -> Int -> Buffer -> Buffer
insertNewlineAt row col buf =
  let line            = getLineAt row buf
      (before, after) = Z.splitAt col line
   in replaceLine row buf [before, after]

