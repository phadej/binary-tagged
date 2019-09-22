{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main (main) where


import Control.Lens          ((&), (.~))
import Data.Bifunctor        (bimap)
import Data.Binary           (Binary, decode, encode)
import Data.Either           (isLeft)
import Data.Monoid           (Product (..), Sum (..))
import Data.Proxy            (Proxy (..))
import GHC.TypeLits          (KnownNat, Nat, natVal)
import Test.QuickCheck
       (Arbitrary (..), Property, counterexample, property, (===))
import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit      (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS8

import Data.Binary.Tagged

import qualified Rec1
import qualified Rec2

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ roundtrips
  , wrongRoundtrips
  , failedRoundtrips
  , testCase "An example hash" $ do
    let hash = structureHash
             $ structure (Proxy :: Proxy [Either (Maybe Char) (Sum Int)])

    Base16.encode hash @?= BS8.pack "c116be43b77d6fe3e79dc76e235f9a46a109a395"
  ]

newtype BinaryTagged (v :: Nat) a = BinaryTagged a deriving (Eq, Show, Binary)

instance (Structured a, KnownNat v) => Structured (BinaryTagged v a) where
    structure _ = structure (Proxy :: Proxy a)
        & typeVersion .~ fromInteger (natVal (Proxy :: Proxy v))

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

failedRoundtrip :: forall a b. (Arbitrary a, Binary a, Binary b, Structured a, Structured b, Show b) => Proxy a -> Proxy b -> a -> Property
failedRoundtrip _ _ x = let x' = bimap trdOf3 trdOf3 $ structuredDecodeOrFail (structuredEncode x) :: Either String b
                        in isLeftProperty x'
