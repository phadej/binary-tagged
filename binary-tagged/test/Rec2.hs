{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Rec2 where

import Data.Binary
import Data.Binary.Instances ()
import Data.Binary.Tagged
import Data.Monoid
import Data.Typeable         (Typeable)
import GHC.Generics          (Generic)
import Test.QuickCheck       (Arbitrary (..))

import Generators

data Rec = Rec (Product Int) (Sum Int)
  deriving (Eq, Show, Generic, Typeable)

instance Binary Rec
instance Structured Rec

instance Arbitrary Rec where
  arbitrary = Rec <$> arbitraryProduct <*> arbitrarySum
