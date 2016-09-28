
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Functor.Coproduct
  ( type (:+:)
  , inl
  , inr
  , (:<:)
  ) where

import Data.Functor.Sum (Sum(..))

infixr 6 :+:

type f :+: g = Sum f g

inl :: f a -> (f :+: g) a
inl = InL

inr :: g a -> (f :+: g) a
inr = InR

class sub :<: sup where
  inj :: sub a -> sup a

instance f :<: f where
  inj = id

instance f :<: (f :+: g) where
  inj = inl

instance g :<: (f :+: g) where
  inj = inr
