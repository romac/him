
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Functor.Coproduct
  ( type (:+:)
  , inl
  , inr
  , coproduct
  , (:<:)
  , inject
  ) where

import Data.Functor.Sum (Sum(..))
import Data.Functor.Coyoneda (Coyoneda, liftCoyoneda)

infixr 6 :+:

type (:+:) = Sum

inl :: f a -> (f :+: g) a
inl = InL

inr :: g a -> (f :+: g) a
inr = InR

coproduct :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
coproduct f _ (InL fa) = f fa
coproduct _ g (InR ga) = g ga

class sub :<: sup where
  inject :: sub a -> sup a

instance f :<: Coyoneda f where
  inject = liftCoyoneda

instance f :<: f where
  inject = id

instance {-# OVERLAPPING #-} f :<: (f :+: g) where
  inject = inl

instance (f :<: h) => f :<: (g :+: h) where
  inject = inr . inject

