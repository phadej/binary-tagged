{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module Rec1 where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Data.Binary
import Data.Binary.Instances ()
import Data.Binary.Tagged
import Data.Monoid
import GHC.Generics
import Test.Tasty.QuickCheck

import Generators

data Rec = Rec (Sum Int) (Product Int)
  deriving (Eq, Show, Generic)

instance Binary Rec
instance Structured Rec

instance Arbitrary Rec where
  arbitrary = Rec <$> arbitrarySum <*> arbitraryProduct
