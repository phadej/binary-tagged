{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.Binary               (Binary, decode, encode)
import Data.Coerce               (coerce)
import Data.Either               (isLeft)
import Data.Functor.Identity     (Identity (..))
import Data.Monoid               (Product (..), Sum (..))
import Data.Proxy                (Proxy (..))
import Data.Singletons.Bool      (SBoolI, reflectBool)
import Data.Typeable             (Typeable)
import GHC.Generics              (Generic)
import Test.QuickCheck
       (Arbitrary (..), Property, counterexample, ioProperty, property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty                (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit          (testCase, (@?=))
import Test.Tasty.QuickCheck     (testProperty)

import Data.Binary.Tagged

import qualified Rec1
import qualified Rec2

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ roundtrips
  , wrongRoundtrips
  , failedRoundtrips
  , testCase "An example hash" $ do
    let hash = structureHash (Proxy :: Proxy [Either (Maybe Char) (Sum Int)])
    hash @?= md5FromInteger 0xbfc698fe96db703b44e5925381902ef2
  ]

proxyRec1 :: Proxy Rec1.Rec
proxyRec1 = Proxy

proxyRec2 :: Proxy Rec2.Rec
proxyRec2 = Proxy

proxyRec1Ver0 :: Proxy (VerTagged 'False Rec1.Rec)
proxyRec1Ver0 = Proxy

proxyRec1Ver1 :: Proxy (VerTagged 'True Rec1.Rec)
proxyRec1Ver1 = Proxy

proxyRec2Ver0 :: Proxy (VerTagged 'False Rec2.Rec)
proxyRec2Ver0 = Proxy

-- proxyRec2Ver1 :: Proxy (VerTagged 'True Rec2.Rec)
-- proxyRec2Ver1 = Proxy

eqRec1Rec2 :: Rec1.Rec -> Rec2.Rec -> Bool
eqRec1Rec2 (Rec1.Rec (Sum a) (Product b)) (Rec2.Rec (Product a') (Sum b')) =
  a == a' && b == b'

roundtrips :: TestTree
roundtrips = testGroup "Roundtrip"
  [ testProperty "Rec1"                $ roundtrip proxyRec1
  , testProperty "Rec2"                $ roundtrip proxyRec2
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

roundtrip :: (Eq a, Show a, Arbitrary a, Binary a, Structured a) => Proxy a -> a -> Property
roundtrip _ x = x === structuredDecode (structuredEncode x)

wrongRoundtrip :: (Arbitrary a, Binary a, Binary b, Structured a, Structured b) => (a -> b -> Bool) -> a -> Property
wrongRoundtrip eq x = property $ eq x $ decode (encode x)

isLeftProperty :: (Show a, Show b) => Either a b -> Property
isLeftProperty x = counterexample ("not isLeft: " ++ show x) (isLeft x)

failedRoundtrip :: forall a b. (Arbitrary a, Binary a, Binary b, Structured a, Structured b, Show b) => Proxy a -> Proxy b -> a -> Property
failedRoundtrip _ _ x = ioProperty $ do
    x' <- structuredDecodeOrFailIO (structuredEncode x) :: IO (Either String b)
    return (isLeftProperty x')

-------------------------------------------------------------------------------
-- VerTagged
-------------------------------------------------------------------------------

newtype VerTagged (v :: Bool) a = VT a deriving (Eq, Show, Generic, Typeable)

instance Binary a => Binary (VerTagged v a)

instance (SBoolI v, Typeable v, Structured a) => Structured (VerTagged v a) where
    structure = set typeVersion (fromBool $ reflectBool (Proxy :: Proxy v)) . genericStructure

fromBool :: Integral a => Bool -> a
fromBool True  = 1
fromBool False = 0

instance Arbitrary a => Arbitrary (VerTagged v a) where
    arbitrary     = fmap VT arbitrary
    shrink (VT x) = fmap VT (shrink x)

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over = coerce

set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l b = over l (const b)
