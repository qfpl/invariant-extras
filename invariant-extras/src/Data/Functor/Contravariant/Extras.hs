module Data.Functor.Contravariant.Extras (
    (>*<)
  , (>|<)
  , Div(..)
  , Dec(..)
  , Chainable(..)
  ) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

infixr 4 >*<
(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided

infixr 3 >|<
(>|<) :: Decidable f => f a -> f b -> f (Either a b)
(>|<) = chosen

infixr 4 >.<
class Contravariant f => Div f where
  (>.<) :: f a -> f b -> f (a, b)

infixr 3 >!<
class Contravariant f => Dec f where
  (>!<) :: f a -> f b -> f (Either a b)

class Decidable f => Chainable f where
  chain :: f a -> (b -> a) -> (a -> f b) -> f b
