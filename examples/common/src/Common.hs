{-# LANGUAGE DeriveGeneric #-}
module Common where

import GHC.Generics

data Identifier = StringId String | IntId Int
  deriving (Eq, Ord, Show, Generic)

data Blob = Blob [Int] Identifier Bool
  deriving (Eq, Ord, Show, Generic)
