{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Rec1 where

import Data.Binary
import Data.Binary.Instances ()
import Data.Binary.Tagged
import Data.Monoid
import Data.Typeable         (Typeable)
import GHC.Generics          (Generic)
import Test.QuickCheck       (Arbitrary (..))

import Generators

data Rec = Rec (Sum Int) (Product Int)
  deriving (Eq, Show, Generic, Typeable)

instance Binary Rec
instance Structured Rec

instance Arbitrary Rec where
  arbitrary = Rec <$> arbitrarySum <*> arbitraryProduct
