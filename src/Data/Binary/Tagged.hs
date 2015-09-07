{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Binary.Tagged
  (
  -- * Data
  BinaryTagged(..),
  binaryTagged,
  NominalSop(..),
  -- * Class
  HasNominalSop(..),
  -- * Generic derivation
  ghcNominalSop,
  ghcNominalType,
  ghcNominalSop1,
  sopNominalSop,
  sopNominalSopS,
  sopNominalType,
  sopNominalTypeS,
  sopNominalSop1,
  sopNominalSop1S,
  -- * Hash
  nominalSopSha1Digest,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.ByteString as BS
import           Data.ByteString.Builder
import           Data.ByteString.Lazy as LBS
import           Data.Digest.Pure.SHA
import           Data.Foldable (Foldable, foldMap)
import           Data.List as List
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.String.UTF8
import           Data.Traversable (Traversable)
import           Generics.SOP as SOP
import           Generics.SOP.GGP as SOP

import qualified GHC.Generics as GHC
import           GHC.TypeLits

-- Instances
import qualified Data.Monoid as Monoid
import           Data.Int
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

-- | 'Binary' serialisable class, which tries to be less error-prone to data structure changes.
--
-- Values are serialised with header consisting of version @v@ and hash of 'nominalSop'.
newtype BinaryTagged (v :: k) a = BinaryTagged { unBinaryTagged :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, GHC.Generic, GHC.Generic1)
-- TODO: Derive Enum, Bounded, Typeable, Data, Hashable, NFData, Numeric classes?

binaryTagged :: Proxy v -> a -> BinaryTagged v a
binaryTagged _ = BinaryTagged

instance Applicative (BinaryTagged v) where
  pure = return
  (<*>) = ap

instance Monad (BinaryTagged v) where
  return = BinaryTagged
  BinaryTagged m >>= k = k m

instance Monoid.Monoid a => Monoid.Monoid (BinaryTagged v a) where
  mempty   = pure Monoid.mempty
  mappend  = liftA2 Monoid.mappend

-- | Version and structure hash are prepended to serialised stream
instance (Binary a, HasNominalSop a, KnownNat v) => Binary (BinaryTagged v a) where
  put (BinaryTagged x) = put ver' >> put hash' >> put x
    where
      proxyV = Proxy :: Proxy v
      proxyA = Proxy :: Proxy a
      ver' = fromIntegral (natVal proxyV) :: Int
      hash' = bytestringDigest . nominalSopSha1Digest . nominalSop $ proxyA

  get = do
      ver <- get
      if ver == ver'
         then do hash <- get
                 if hash == hash'
                    then fmap BinaryTagged get
                    else fail $ "Non matching structure hashes: got" <> show hash <> "; expected: " <> show hash'
         else fail $ "Non matching versions: got " <> show ver <> "; expected: " <> show ver'
    where
      proxyV = Proxy :: Proxy v
      proxyA = Proxy :: Proxy a
      ver' = fromIntegral (natVal proxyV) :: Int
      hash' = bytestringDigest . nominalSopSha1Digest . nominalSop $ proxyA

-- | Data type structure, with nominal information.
data NominalSop = NominalType String
                | NominalNewtype String NominalSop
                | NominalSop String [[NominalSop]]
  deriving (Eq, Ord, Show, GHC.Generic)

-- | Type class providing `NominalSop` for each data type.
--
-- For regular non-recursive ADTs 'HasNominalSop' can be derived generically.
--
-- > data Record = Record { a :: Int, b :: Bool, c :: [Char] } deriving (Generic)
-- > instance hasNominalSop Record
--
-- For stable types, you can provide only type name
--
-- > instance HasNominalSop Int where nominalSop = ghcNominalType -- infer name from Generic information
-- > instance HasNominalSop Integer where nominalSop _ = NominalType "Integer"
--
-- Recursive type story is a bit sad atm. If the type structure is stable, you can do:
--
-- > instance HasNominalSop a => HasNominalSop [a] where nominalSop = ghcNominalSop1
class HasNominalSop a where
  nominalSop :: Proxy a -> NominalSop

  default nominalSop :: (GHC.Generic a, All2 HasNominalSop (GCode a), GDatatypeInfo a, SingI (GCode a)) => Proxy a -> NominalSop
  nominalSop = ghcNominalSop

toUtf8ByteString :: String -> BS.ByteString
toUtf8ByteString = toRep . fromString

nominalSopBuilder :: NominalSop -> Builder
nominalSopBuilder (NominalType name) =
  byteString "type" <> byteString (toUtf8ByteString name)
nominalSopBuilder (NominalNewtype name nsop) =
  byteString "newtype" <> byteString (toUtf8ByteString name) <> nominalSopBuilder nsop
nominalSopBuilder (NominalSop name nsops) =
  byteString "adt" <> byteString (toUtf8ByteString name) <> intDec (List.length nsops) <> foldMap nominalSopBuilder' nsops

nominalSopBuilder' :: [NominalSop] -> Builder
nominalSopBuilder' nsops = intDec (List.length nsops) <> foldMap nominalSopBuilder nsops

nominalSopByteString :: NominalSop -> LBS.ByteString
nominalSopByteString = toLazyByteString . nominalSopBuilder

nominalSopSha1Digest :: NominalSop -> Digest SHA1State
nominalSopSha1Digest = sha1 . nominalSopByteString

-- Generic derivation

ghcNominalSop :: (GHC.Generic a, All2 HasNominalSop (GCode a), GDatatypeInfo a, SingI (GCode a)) => Proxy a -> NominalSop
ghcNominalSop proxy = sopNominalSopS (gdatatypeInfo proxy)

ghcNominalType ::  (GHC.Generic a,  GDatatypeInfo a) => Proxy a -> NominalSop
ghcNominalType proxy = sopNominalTypeS (gdatatypeInfo proxy)

ghcNominalSop1 :: forall f a. (GHC.Generic1 f, GDatatypeInfo (f a), HasNominalSop a) => Proxy (f a) -> NominalSop
ghcNominalSop1 proxy = sopNominalSop1S (nominalSop (Proxy :: Proxy a)) (gdatatypeInfo proxy)

-- SOP derivation

sopNominalSop :: forall a. (Generic a, HasDatatypeInfo a, All2 HasNominalSop (Code a)) => Proxy a -> NominalSop
sopNominalSop proxy = sopNominalSopS (datatypeInfo proxy)

sopNominalSopS :: forall xss. (All2 HasNominalSop xss, SingI xss) => DatatypeInfo xss -> NominalSop
sopNominalSopS di@(Newtype _ _ ci)  = NominalNewtype (datatypeName di) (sopNominalNewtype ci)
sopNominalSopS di@(ADT _ _ _)       = NominalSop (datatypeName di) (sopNominalAdt (toNP' (sing :: Sing xss)))

sopNominalNewtype :: forall x. HasNominalSop x => ConstructorInfo '[x] -> NominalSop
sopNominalNewtype _ = nominalSop (Proxy :: Proxy x)

sopNominalAdt :: (All2 HasNominalSop xss) => NP (NP Proxy) xss -> [[NominalSop]]
sopNominalAdt Nil          = []
sopNominalAdt (p :* ps)  = sopNominalSopP p : sopNominalAdt ps

sopNominalSopP :: (All HasNominalSop xs) => NP Proxy xs -> [NominalSop]
sopNominalSopP Nil = []
sopNominalSopP (proxy :* rest) =  nominalSop proxy : sopNominalSopP rest

sopNominalType :: forall a. (Generic a, HasDatatypeInfo a) => Proxy a -> NominalSop
sopNominalType proxy = sopNominalTypeS (datatypeInfo proxy)

sopNominalTypeS :: DatatypeInfo xss -> NominalSop
sopNominalTypeS di = NominalType (datatypeName di)

sopNominalSop1 :: forall f a. (Generic (f a), HasDatatypeInfo (f a), HasNominalSop a) => Proxy (f a) -> NominalSop
sopNominalSop1 proxy = sopNominalSop1S (nominalSop (Proxy :: Proxy a)) (datatypeInfo proxy)

sopNominalSop1S :: NominalSop -> DatatypeInfo xss -> NominalSop
sopNominalSop1S nsop di = NominalNewtype (datatypeName di) nsop

-- SOP helpers

datatypeName :: DatatypeInfo xss -> DatatypeName
datatypeName (Newtype _ d _)  = d
datatypeName (ADT _ d _)      = d

toNP :: Sing xs -> NP Proxy xs
toNP SNil = Nil
toNP SCons = Proxy :* toNP sing

toNP' :: Sing xss -> NP (NP Proxy) xss
toNP' SNil = Nil
toNP' SCons = toNP sing :* toNP' sing

-- Instances

instance HasNominalSop Bool where nominalSop = ghcNominalType
instance HasNominalSop Char where nominalSop = ghcNominalType
instance HasNominalSop Int where nominalSop = ghcNominalType
instance HasNominalSop Integer where nominalSop _ = NominalType "Integer"

instance HasNominalSop Int8 where nominalSop _ = NominalType "Int8"
instance HasNominalSop Int16 where nominalSop _ = NominalType "Int16"
instance HasNominalSop Int32 where nominalSop _ = NominalType "Int32"
instance HasNominalSop Int64 where nominalSop _ = NominalType "Int64"

instance HasNominalSop Word8 where nominalSop _ = NominalType "Word8"
instance HasNominalSop Word16 where nominalSop _ = NominalType "Word16"
instance HasNominalSop Word32 where nominalSop _ = NominalType "Word32"
instance HasNominalSop Word64 where nominalSop _ = NominalType "Word64"

-- Recursive types
instance HasNominalSop a => HasNominalSop [a] where nominalSop = ghcNominalSop1

instance HasNominalSop a => HasNominalSop (Maybe a)
instance (HasNominalSop a, HasNominalSop b) => HasNominalSop (Either a b)

-- Monoid
instance HasNominalSop a => HasNominalSop (Monoid.Sum a)
instance HasNominalSop a => HasNominalSop (Monoid.Product a)
-- TODO: add more

-- ByteString
instance HasNominalSop BS.ByteString where nominalSop _ = NominalType "ByteString.Strict"
instance HasNominalSop LBS.ByteString where nominalSop _ = NominalType "ByteString.Lazy"

-- Text
instance HasNominalSop S.Text where nominalSop _ = NominalType "Text.Strict"
instance HasNominalSop L.Text where nominalSop _ = NominalType "Text.Lazy"

-- Containers

-- Unordered containers

-- Array

-- Vector

-- Value
