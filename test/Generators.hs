module Generators where

import Data.Monoid
import Test.Tasty.QuickCheck

arbitrarySum :: Arbitrary a => Gen (Sum a)
arbitrarySum = fmap Sum arbitrary

arbitraryProduct :: Arbitrary a => Gen (Product a)
arbitraryProduct = fmap Product arbitrary
