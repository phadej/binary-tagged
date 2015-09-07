{-# LANGUAGE DeriveGeneric #-}
module Rec2 where

import Control.Applicative
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Monoid
import GHC.Generics
import Test.Tasty.QuickCheck

import Generators

data Rec = Rec (Product Int) (Sum Int)
  deriving (Eq, Show, Generic)

instance Binary Rec
instance HasStructuralInfo Rec
instance HasSemanticVersion Rec

instance Arbitrary Rec where
  arbitrary = Rec <$> arbitraryProduct <*> arbitrarySum
