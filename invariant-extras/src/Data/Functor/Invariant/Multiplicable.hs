module Data.Functor.Invariant.Multiplicable (
    Multiply(..)
  , Multiplicable(..)
  , Factor(..)
  , Factorable(..)
  , Dependable(..)
  ) where

import Data.Functor.Invariant
import Data.Functor.Product
import Data.Void

import Unsafe.Coerce (unsafeCoerce)

-- this is temporary
import Generics.Eot (Void)

infixr 4 >>*<<
infixr 4 >>*
infixr 4 *<<

class Invariant f => Multiply f where
  (>>*<<) :: f a -> f b -> f (a, b)

  (>>*) :: f a -> f () -> f a
  fa >>* fu = invmap fst (\x -> (x, ())) (fa >>*<< fu)

  (*<<) :: f () -> f b -> f b
  fu *<< fb = invmap snd (\x -> ((), x)) (fu >>*<< fb)

instance (Multiply f, Multiply g) => Multiply (Product f g) where
  Pair f1 g1 >>*<< Pair f2 g2 = Pair (f1 >>*<< f2) (g1 >>*<< g2)

class Multiply f => Multiplicable f where
  munit :: a -> f a

instance (Multiplicable f, Multiplicable g) => Multiplicable (Product f g) where
  munit x = Pair (munit x) (munit x)

infixr 3 >>|<<

class Invariant f => Factor f where
  (>>|<<) :: f a -> f b -> f (Either a b)

instance (Factor f, Factor g) => Factor (Product f g) where
  Pair f1 g1 >>|<< Pair f2 g2 = Pair (f1 >>|<< f2) (g1 >>|<< g2)

class Factor f => Factorable f where
  funit :: (a -> Generics.Eot.Void) -> f a

  plug :: f a
  plug = funit unsafeCoerce

instance (Factorable f, Factorable g) => Factorable (Product f g) where
  funit f = Pair (funit f) (funit f)

class Multiplicable f => Dependable f where
  depend :: f a -> (b -> a) -> (a -> f b) -> f b
