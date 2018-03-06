module Data.Functor.Invariant.Invtraversable (
    Invtraversable(..)
  ) where

import Data.Functor.Invariant
import Data.Functor.Invariant.Multiplicable

class Functor t => Invtraversable t where
  invsequence :: Multiplicable f => t (f a) -> f (t a)
  invsequence = invtraverse id

  invtraverse :: Multiplicable f => (a -> f b) -> t a -> f (t b)
  invtraverse f = invsequence . fmap f

instance Invtraversable [] where
  invsequence [] = munit []
  invsequence (x : xs) =
    let
      f (h, t) = h : t
      g [] = error "should not happen"
      g (h : t) = (h, t)
    in
      invmap f g $ x >>*<< invsequence xs
