
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.FreeC
  ( type (~>)
  , FreeC
  , transC
  , liftFreeC
  , foldFreeC
  ) where

import Control.Monad.Free    (Free, liftF, foldFree)
import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda, lowerCoyoneda)

type f ~> g = forall x. f x -> g x

transC :: (f ~> g) -> (Coyoneda f ~> Coyoneda g)
transC n (Coyoneda k fa) = Coyoneda k (n fa)

type FreeC f = Free (Coyoneda f)

liftFreeC :: f a -> FreeC f a
liftFreeC = liftF . liftCoyoneda

foldFreeC :: Monad m => (f ~> m) -> FreeC f a -> m a
foldFreeC n = foldFree (lowerCoyoneda . transC n)

