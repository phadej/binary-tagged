{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import           Data.Bifunctor
import           Data.Binary
import           Data.Binary.Tagged
import           Data.Either
import           Data.Monoid
import           Data.Proxy
import           Test.Tasty
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.QuickCheck

import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Base16.Lazy as Base16 

import qualified Rec1
import qualified Rec2

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ roundtrips
  , wrongRoundtrips
  , failedRoundtrips
  , testProperty "Interleave" interleaveProp
  , testCase "An example hash" $ do
    let hash = structuralInfoSha1ByteStringDigest
             $ structuralInfo (Proxy :: Proxy [Either (Maybe Char) (Sum Int)])

    Base16.encode hash @?= BS8.pack "acff3d40f6f06f87b4da8d3d3eb5682251867cc5"
  ]

-- | We actually check that this compiles.
interleaveProp :: Property
interleaveProp = property $ once $ lhs === rhs
  where lhs :: Proxy 7
        lhs = Proxy
        rhs :: Proxy (Interleave 2 1)
        rhs = Proxy

instance Arbitrary a => Arbitrary (BinaryTagged v a) where
  arbitrary = fmap BinaryTagged arbitrary

proxyRec1 :: Proxy Rec1.Rec
proxyRec1 = Proxy

proxyRec1Ver0 :: Proxy (BinaryTagged 0 Rec1.Rec)
proxyRec1Ver0 = Proxy

proxyRec1Ver1 :: Proxy (BinaryTagged 1 Rec1.Rec)
proxyRec1Ver1 = Proxy

proxyRec2 :: Proxy Rec2.Rec
proxyRec2 = Proxy

proxyRec2Ver0 :: Proxy (BinaryTagged 0 Rec2.Rec)
proxyRec2Ver0 = Proxy

proxyRec2Ver1 :: Proxy (BinaryTagged 1 Rec2.Rec)
proxyRec2Ver1 = Proxy

eqRec1Rec2 :: Rec1.Rec -> Rec2.Rec -> Bool
eqRec1Rec2 (Rec1.Rec (Sum a) (Product b)) (Rec2.Rec (Product a') (Sum b')) =
  a == a' && b == b'

roundtrips :: TestTree
roundtrips = testGroup "Roundtrip"
  [ testProperty "Rec1"                $ roundtrip proxyRec1
  , testProperty "BinaryTagged 0 Rec1" $ roundtrip proxyRec1Ver0
  , testProperty "BinaryTagged 1 Rec1" $ roundtrip proxyRec1Ver1
  , testProperty "Rec2"                $ roundtrip proxyRec2
  , testProperty "BinaryTagged 0 Rec2" $ roundtrip proxyRec2Ver0
  , testProperty "BinaryTagged 1 Rec2" $ roundtrip proxyRec2Ver1
  ]

wrongRoundtrips :: TestTree
wrongRoundtrips = testGroup "Decode successful, data invalid"
  [ testProperty "Rec1 -> Rec2" $ wrongRoundtrip eqRec1Rec2
  , testProperty "Rec2 -> Rec1" $ wrongRoundtrip eqRec1Rec2
  ]

failedRoundtrips :: TestTree
failedRoundtrips = testGroup "Failed roundtrips"
  [ testProperty "Different version" $ failedRoundtrip proxyRec1Ver0 proxyRec1Ver1
  , testProperty "Different structure" $ failedRoundtrip proxyRec1Ver0 proxyRec2Ver0
  ]

roundtrip :: (Eq a, Show a, Arbitrary a, Binary a) => Proxy a -> a -> Property
roundtrip _ x = x === decode (encode x)

wrongRoundtrip :: (Arbitrary a, Binary a, Binary b) => (a -> b -> Bool) -> a -> Property
wrongRoundtrip eq x = property $ eq x $ decode (encode x)

trdOf3 :: (a, b, c) -> c
trdOf3 (_, _, c) = c

isLeftProperty :: (Show a, Show b) => Either a b -> Property
isLeftProperty x = counterexample ("not isLeft: " <> show x) (isLeft x)

failedRoundtrip :: forall a b. (Arbitrary a, Binary a, Binary b, Show b) => Proxy a -> Proxy b -> a -> Property
failedRoundtrip _ _ x = let x' = bimap trdOf3 trdOf3 $ decodeOrFail (encode x) :: Either String b
                        in isLeftProperty x'
