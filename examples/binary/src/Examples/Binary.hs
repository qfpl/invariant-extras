{-# LANGUAGE GADTs #-}
module Examples.Binary where

import Data.Void

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Extras

import Data.Functor.Invariant.Multiplicable
import Data.Functor.Invariant.Product
import Data.Functor.Invariant.Generic
import Data.Functor.Invariant.Invtraversable

import Data.Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LBS

import Common

newtype Decoder a = Decoder { runDecoder :: a -> Put }

instance Contravariant Decoder where
  contramap ba (Decoder f) = Decoder (f . ba)

instance Divisible Decoder where
  conquer = Decoder (const mempty)
  divide split db dc = Decoder $ \a ->
    case split a of
      (b, c) -> runDecoder db b >> runDecoder dc c

instance Decidable Decoder where
  lose f = Decoder (absurd . f)
  choose split db dc = Decoder $ \a ->
    case split a of
      Left b -> runDecoder db b
      Right c -> runDecoder dc c

instance Chainable Decoder where
  chain fa ba afb = Decoder $ \b ->
    runDecoder fa (ba b) >> runDecoder (afb (ba b)) b

type Transcoder a = ProductFC Get Decoder a

transcodeGet :: Transcoder a -> LBS.ByteString -> a
transcodeGet (ProductFC g _) b = runGet g b

transcodePut :: Transcoder a -> a -> LBS.ByteString
transcodePut (ProductFC _ (Decoder f)) a = runPut $ f a

defaultTranscoder :: Binary a => Transcoder a
defaultTranscoder = ProductFC get (Decoder put)

transcodeInt :: Transcoder Int
transcodeInt = defaultTranscoder

transcodeString :: Transcoder String
transcodeString = defaultTranscoder

transcodeBool :: Transcoder Bool
transcodeBool = defaultTranscoder

transcodeList :: Transcoder a -> Transcoder [a]
transcodeList ta =
  depend transcodeInt length (\n -> invsequence (replicate n ta))

transcodeIdentifier' :: Int -> Transcoder Identifier
transcodeIdentifier' i = eotSum $
  (if i == 0 then transcodeString else plug) >>*<< munit () >>|<<
  (if i == 1 then transcodeInt else plug)    >>*<< munit () >>|<<
  funit id

transcodeIdentifier :: Transcoder Identifier
transcodeIdentifier =
  let
    back (StringId _)= 0
    back (IntId _)= 1
  in
    depend transcodeInt back transcodeIdentifier'

transcodeBlob :: Transcoder Blob
transcodeBlob = eotProduct $
  transcodeList transcodeInt >>*<<
  transcodeIdentifier >>*<<
  transcodeBool >>*<<
  munit ()
