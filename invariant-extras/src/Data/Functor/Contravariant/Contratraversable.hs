module Data.Functor.Contravariant.Contratraversable (
    Contratraversable(..)
  ) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

class Functor t => Contratraversable t where
  contrasequence :: Divisible f => t (f a) -> f (t a)
  contrasequence = contratraverse id

  contratraverse :: Divisible f => (a -> f b) -> t a -> f (t b)
  contratraverse f = contrasequence . fmap f

instance Contratraversable [] where
  contrasequence [] = conquer
  contrasequence (x : xs) =
    let
      g [] = error "should not happen"
      g (h : t) = (h, t)
    in
      contramap g $ divided x (contrasequence xs)

