{-# LANGUAGE DeriveGeneric #-}
module Rec1 where

import Control.Applicative
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Monoid
import GHC.Generics
import Test.Tasty.QuickCheck

import Generators

data Rec = Rec (Sum Int) (Product Int)
  deriving (Eq, Show, Generic)

instance Binary Rec
instance HasNominalSop Rec

instance Arbitrary Rec where
  arbitrary = Rec <$> arbitrarySum <*> arbitraryProduct
