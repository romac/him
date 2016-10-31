
{-# LANGUAGE OverloadedStrings #-}

module Him.Buffer where

import           Data.Maybe (fromMaybe)

import qualified Data.Text        as T
import qualified Data.Text.Zipper as Z

newtype Buffer = Buffer (Z.TextZipper T.Text)
  deriving (Eq, Show)

empty :: Buffer
empty = Buffer (Z.textZipper [T.empty] Nothing)

