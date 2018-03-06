{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
module Data.Functor.Invariant.Generic (
    eotSum
  , eotProduct
  ) where

import Data.Void
import GHC.Generics

import Data.Functor.Invariant
import Generics.Eot

import Data.Functor.Invariant.Multiplicable

eotSum :: (HasEot a, Invariant f) => f (Eot a) -> f a
eotSum = invmap fromEot toEot

otherAbsurd :: Generics.Eot.Void -> a
otherAbsurd a = case a of {}

eotProduct :: (HasEot a, Invariant f, Factorable f, Eot a ~ Either b Generics.Eot.Void) => f b -> f a
eotProduct x = invmap (fromEot . fmap absurd) (fmap otherAbsurd . toEot) (x >>|<< funit)
