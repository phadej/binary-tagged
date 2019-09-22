{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module Rec2 where

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

data Rec = Rec (Product Int) (Sum Int)
  deriving (Eq, Show, Generic)

instance Binary Rec
instance Structured Rec

instance Arbitrary Rec where
  arbitrary = Rec <$> arbitraryProduct <*> arbitrarySum
