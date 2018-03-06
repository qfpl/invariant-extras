module Data.Functor.Invariant.Product (
    ProductFC(..)
  , ProductCF(..)
  , ProductFI(..)
  , ProductIF(..)
  , ProductCI(..)
  , ProductIC(..)
  ) where

import Data.Functor.Contravariant
import Data.Functor.Invariant

import Control.Applicative

import Data.Functor.Contravariant.Divisible

import Data.Functor.Contravariant.Extras
import Data.Functor.Invariant.Multiplicable

data ProductFC f g a = ProductFC (f a) (g a)

instance (Functor f, Contravariant g) => Invariant (ProductFC f g) where
  invmap ab ba (ProductFC fa ga) =
    ProductFC (fmap ab fa) (contramap ba ga)

instance (Applicative f, Divisible g) => Multiply (ProductFC f g) where
  ProductFC f1 g1 >>*<< ProductFC f2 g2 =
    ProductFC ((,) <$> f1 <*> f2) (g1 >*< g2)

instance (Applicative f, Divisible g) => Multiplicable (ProductFC f g) where
  munit a =
    ProductFC (pure a) conquer

instance (Alternative f, Decidable g) => Factor (ProductFC f g) where
  ProductFC f1 g1 >>|<< ProductFC f2 g2 =
    ProductFC (Left <$> f1 <|> Right <$> f2) (g1 >|< g2)

instance (Alternative f, Decidable g) => Factorable (ProductFC f g) where
  funit =
    ProductFC empty lost

instance (Monad f, Chainable g) => Dependable (ProductFC f g) where
  depend (ProductFC fa ga) ba afb =
    let
      pfcFst (ProductFC f _) = f
      pfcSnd (ProductFC _ s) = s
    in
      ProductFC (fa >>= pfcFst . afb) (chain ga ba (pfcSnd . afb))

data ProductCF f g a = ProductCF (f a) (g a)

instance (Contravariant f, Functor g) => Invariant (ProductCF f g) where
  invmap ab ba (ProductCF fa ga) =
    ProductCF (contramap ba fa) (fmap ab ga)

instance (Divisible f, Applicative g) => Multiply (ProductCF f g) where
  ProductCF f1 g1 >>*<< ProductCF f2 g2 =
    ProductCF (f1 >*< f2) ((,) <$> g1 <*> g2)

instance (Divisible f, Applicative g) => Multiplicable (ProductCF f g) where
  munit a =
    ProductCF conquer (pure a)

instance (Decidable f, Alternative g) => Factor (ProductCF f g) where
  ProductCF f1 g1 >>|<< ProductCF f2 g2 =
    ProductCF (f1 >|< f2) (Left <$> g1 <|> Right <$> g2)

instance (Decidable f, Alternative g) => Factorable (ProductCF f g) where
  funit =
    ProductCF lost empty

instance (Chainable f, Monad g) => Dependable (ProductCF f g) where
  depend (ProductCF fa ga) ba afb =
    let
      pcfFst (ProductCF f _) = f
      pcfSnd (ProductCF _ s) = s
    in
      ProductCF (chain fa ba (pcfFst . afb)) (ga >>= pcfSnd . afb)

data ProductFI f g a = ProductFI (f a) (g a)

instance (Functor f, Invariant g) => Invariant (ProductFI f g) where
  invmap ab ba (ProductFI fa ga) =
    ProductFI (fmap ab fa) (invmap ab ba ga)

instance (Applicative f, Multiply g) => Multiply (ProductFI f g) where
  ProductFI f1 g1 >>*<< ProductFI f2 g2 =
    ProductFI ((,) <$> f1 <*> f2) (g1 >>*<< g2)

instance (Applicative f, Multiplicable g) => Multiplicable (ProductFI f g) where
  munit a =
    ProductFI (pure a) (munit a)

instance (Alternative f, Factor g) => Factor (ProductFI f g) where
  ProductFI f1 g1 >>|<< ProductFI f2 g2 =
    ProductFI (Left <$> f1 <|> Right <$> f2) (g1 >>|<< g2)

instance (Alternative f, Factorable g) => Factorable (ProductFI f g) where
  funit =
    ProductFI empty funit

instance (Monad f, Dependable g) => Dependable (ProductFI f g) where
  depend (ProductFI fa ga) ba afb =
    let
      pfiFst (ProductFI f _) = f
      pfiSnd (ProductFI _ s) = s
    in
      ProductFI (fa >>= pfiFst . afb) (depend ga ba (pfiSnd . afb))

data ProductIF f g a = ProductIF (f a) (g a)

instance (Invariant f, Functor g) => Invariant (ProductIF f g) where
  invmap ab ba (ProductIF fa ga) =
    ProductIF (invmap ab ba fa) (fmap ab ga)

instance (Multiply f, Applicative g) => Multiply (ProductIF f g) where
  ProductIF f1 g1 >>*<< ProductIF f2 g2 =
    ProductIF (f1 >>*<< f2) ((,) <$> g1 <*> g2)

instance (Multiplicable f, Applicative g) => Multiplicable (ProductIF f g) where
  munit a =
    ProductIF (munit a) (pure a)

instance (Factor f, Alternative g) => Factor (ProductIF f g) where
  ProductIF f1 g1 >>|<< ProductIF f2 g2 =
    ProductIF (f1 >>|<< f2) (Left <$> g1 <|> Right <$> g2)

instance (Factorable f, Alternative g) => Factorable (ProductIF f g) where
  funit =
    ProductIF funit empty

instance (Dependable f, Monad g) => Dependable (ProductIF f g) where
  depend (ProductIF fa ga) ba afb =
    let
      pifFst (ProductIF f _) = f
      pifSnd (ProductIF _ s) = s
    in
      ProductIF (depend fa ba (pifFst . afb)) (ga >>= pifSnd . afb)

data ProductCI f g a = ProductCI (f a) (g a)

instance (Contravariant f, Invariant g) => Invariant (ProductCI f g) where
  invmap ab ba (ProductCI fa ga) =
    ProductCI (contramap ba fa) (invmap ab ba ga)

instance (Divisible f, Multiply g) => Multiply (ProductCI f g) where
  ProductCI f1 g1 >>*<< ProductCI f2 g2 =
    ProductCI (f1 >*< f2) (g1 >>*<< g2)

instance (Divisible f, Multiplicable g) => Multiplicable (ProductCI f g) where
  munit a =
    ProductCI conquer (munit a)

instance (Decidable f, Factor g) => Factor (ProductCI f g) where
  ProductCI f1 g1 >>|<< ProductCI f2 g2 =
    ProductCI (f1 >|< f2) (g1 >>|<< g2)

instance (Decidable f, Factorable g) => Factorable (ProductCI f g) where
  funit =
    ProductCI lost funit

instance (Chainable f, Dependable g) => Dependable (ProductCI f g) where
  depend (ProductCI fa ga) ba afb =
    let
      pciFst (ProductCI f _) = f
      pciSnd (ProductCI _ s) = s
    in
      ProductCI (chain fa ba (pciFst . afb)) (depend ga ba (pciSnd . afb))

data ProductIC f g a = ProductIC (f a) (g a)

instance (Invariant f, Contravariant g) => Invariant (ProductIC f g) where
  invmap ab ba (ProductIC fa ga) =
    ProductIC (invmap ab ba fa) (contramap ba ga)

instance (Multiply f, Divisible g) => Multiply (ProductIC f g) where
  ProductIC f1 g1 >>*<< ProductIC f2 g2 =
    ProductIC (f1 >>*<< f2) (g1 >*< g2)

instance (Multiplicable f, Divisible g) => Multiplicable (ProductIC f g) where
  munit a =
    ProductIC (munit a) conquer

instance (Factor f, Decidable g) => Factor (ProductIC f g) where
  ProductIC f1 g1 >>|<< ProductIC f2 g2 =
    ProductIC (f1 >>|<< f2) (g1 >|< g2)

instance (Factorable f, Decidable g) => Factorable (ProductIC f g) where
  funit =
    ProductIC funit lost

instance (Dependable f, Chainable g) => Dependable (ProductIC f g) where
  depend (ProductIC fa ga) ba afb =
    let
      picFst (ProductIC f _) = f
      picSnd (ProductIC _ s) = s
    in
      ProductIC (depend fa ba (picFst . afb)) (chain ga ba (picSnd . afb))
