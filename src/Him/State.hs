
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Him.State where

import           Data.Functor.Coproduct
import           Control.Monad.FreeC

import           Him.Buffer (Buffer)
import qualified Him.Buffer as Buffer

data State = State
  { buffer :: Buffer }
  deriving (Eq, Show)

mkBlankState :: State
mkBlankState = State (Buffer.mkEmptyBuffer)

data StateF a where
  Blank     :: StateF State
  GetBuffer :: StateF Buffer

blank :: StateF :<: f => FreeC f State
blank = injectFreeC Blank

getBuffer :: StateF :<: f => FreeC f Buffer
getBuffer = injectFreeC GetBuffer

