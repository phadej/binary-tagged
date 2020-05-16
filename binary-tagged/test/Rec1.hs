{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Rec1 where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

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
