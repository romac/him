
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.FreeC
  ( type (~>)
  , FreeC
  , liftNatC
  , liftFreeC
  , injectFreeC
  , injectFree
  , foldFreeC
  ) where

import Control.Monad.Free    (Free, liftF, foldFree)
import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda, lowerCoyoneda)

import Data.Functor.Coproduct ((:<:), inject)

type f ~> g = forall x. f x -> g x

liftNatC :: (f ~> g) -> (Coyoneda f ~> Coyoneda g)
liftNatC n (Coyoneda k fa) = Coyoneda k (n fa)

injectFree :: (f :<: g, Functor g) => f ~> Free g
injectFree = liftF . inject

type FreeC f = Free (Coyoneda f)

liftFreeC :: f ~> FreeC f
liftFreeC = liftF . liftCoyoneda

injectFreeC :: f :<: g => f ~> FreeC g
injectFreeC = liftFreeC . inject

foldFreeC :: Monad m => (f ~> m) -> FreeC f ~> m
foldFreeC n = foldFree (lowerCoyoneda . liftNatC n)

