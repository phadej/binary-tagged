{-# LANGUAGE CPP #-}
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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- We need this for Interleave
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Tagged
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Structurally tag binary serialisation stream.
--
-- Say you have:
--
-- > data Record = Record
-- >   { _recordFields :: HM.HashMap Text (Integer, ByteString)
-- >   , _recordEnabled :: Bool
-- >   }
-- >   deriving (Eq, Show, Generic)
-- >
-- > instance Binary Record
-- > instance HasStructuralInfo Record
-- > instance HasSemanticVersion Record
--
-- then you can serialise and deserialise @Record@ values with a structure tag by simply
--
-- > encodeTaggedFile "cachefile" record
-- > decodeTaggedFile "cachefile" :: IO Record
--
-- If structure of @Record@ changes in between, deserialisation will fail early.
module Data.Binary.Tagged
  (
  -- * Data
  BinaryTagged(..),
  BinaryTagged',
  binaryTag,
  binaryTag',
  binaryUntag,
  binaryUntag',
  StructuralInfo(..),
  -- * Serialisation
  taggedEncode,
  taggedDecode,
  taggedDecodeOrFail,
  -- * IO functions for serialisation
  taggedEncodeFile,
  taggedDecodeFile,
  taggedDecodeFileOrFail,
  -- * Class
  HasStructuralInfo(..),
  HasSemanticVersion(..),
  Version,
  -- ** Type level calculations
  Interleave,
  SumUpTo,
  Div2,
  -- * Generic derivation
  -- ** GHC
  ghcStructuralInfo,
  ghcNominalType,
  ghcStructuralInfo1,
  -- ** SOP
  sopStructuralInfo,
  sopNominalType,
  sopStructuralInfo1,
  -- ** SOP direct
  sopStructuralInfoS,
  sopNominalTypeS,
  sopStructuralInfo1S,
  -- * Hash
  structuralInfoSha1Digest,
  structuralInfoSha1ByteStringDigest,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get (ByteOffset)
import           Data.ByteString as BS
import           Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16.Lazy as Base16
import           Data.Digest.Pure.SHA
import           Data.Monoid ((<>))
import           Data.Proxy
import           Generics.SOP as SOP
import           Generics.SOP.GGP as SOP

#if MIN_VERSION_generics_sop(0,2,0)
import           Generics.SOP.Constraint as SOP
#endif

#if !MIN_VERSION_base(4,8,0)
import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)
#endif

import qualified GHC.Generics as GHC
import           GHC.TypeLits

-- Instances
import           Data.Int
import qualified Data.Array.IArray as Array
import qualified Data.Array.Unboxed as Array
import qualified Data.Fixed as Fixed
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Ratio as Ratio
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Version as Version
import qualified Numeric.Natural as Natural

#ifdef MIN_VERSION_aeson
import qualified Data.Aeson as Aeson
#endif

-- | 'Binary' serialisable class, which tries to be less error-prone to data structure changes.
--
-- Values are serialised with header consisting of version @v@ and hash of 'structuralInfo'.
newtype BinaryTagged (v :: k) a = BinaryTagged { unBinaryTagged :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, GHC.Generic, GHC.Generic1)
-- TODO: Derive Enum, Bounded, Typeable, Data, Hashable, NFData, Numeric classes?

type BinaryTagged' a = BinaryTagged (SemanticVersion a) a

binaryTag :: Proxy v -> a -> BinaryTagged v a
binaryTag _ = BinaryTagged

binaryTag' :: HasSemanticVersion a => a -> BinaryTagged' a
binaryTag' = BinaryTagged

binaryUntag :: Proxy v -> BinaryTagged v a -> a
binaryUntag _ = unBinaryTagged

binaryUntag' :: HasSemanticVersion a => BinaryTagged' a -> a
binaryUntag' = unBinaryTagged

-- | Tagged version of 'encode'
taggedEncode ::  forall a. (HasStructuralInfo a, HasSemanticVersion a, Binary a) => a -> LBS.ByteString
taggedEncode = encode . binaryTag (Proxy :: Proxy (SemanticVersion a))

-- | Tagged version of 'decode'
taggedDecode :: forall a. (HasStructuralInfo a, HasSemanticVersion a, Binary a) => LBS.ByteString -> a
taggedDecode = binaryUntag (Proxy :: Proxy (SemanticVersion a)) . decode

-- | Tagged version of 'decodeOrFail'
taggedDecodeOrFail :: forall a. (HasStructuralInfo a, HasSemanticVersion a, Binary a)
                   => LBS.ByteString
                   -> Either (LBS.ByteString, ByteOffset, String) (LBS.ByteString, ByteOffset, a)
taggedDecodeOrFail = fmap3 (binaryUntag (Proxy :: Proxy (SemanticVersion a))) . decodeOrFail
  where fmap3 f = fmap (\(a, b, c) -> (a, b, f c))

-- | Tagged version of 'encodeFile'
taggedEncodeFile :: forall a. (HasStructuralInfo a, HasSemanticVersion a, Binary a) => FilePath -> a -> IO ()
taggedEncodeFile filepath = encodeFile filepath . binaryTag (Proxy :: Proxy (SemanticVersion a))

-- | Tagged version of 'decodeFile'
taggedDecodeFile :: forall a. (HasStructuralInfo a, HasSemanticVersion a, Binary a) => FilePath -> IO a
taggedDecodeFile = fmap (binaryUntag (Proxy :: Proxy (SemanticVersion a))) . decodeFile

-- | Tagged version of 'decodeFileOrFail'
taggedDecodeFileOrFail :: forall a. (HasStructuralInfo a, HasSemanticVersion a, Binary a) => FilePath -> IO (Either (ByteOffset, String) a)
taggedDecodeFileOrFail = (fmap . fmap) (binaryUntag (Proxy :: Proxy (SemanticVersion a))) . decodeFileOrFail

instance Applicative (BinaryTagged v) where
  pure = return
  (<*>) = ap

instance Monad (BinaryTagged v) where
  return = BinaryTagged
  BinaryTagged m >>= k = k m

instance Semigroup.Semigroup a => Semigroup.Semigroup (BinaryTagged v a) where
  (<>) = liftA2 (Semigroup.<>)

instance Monoid.Monoid a => Monoid.Monoid (BinaryTagged v a) where
  mempty   = pure Monoid.mempty
  mappend  = liftA2 Monoid.mappend

-- | Type the semantic version is serialised with.
type Version = Word32

-- | Version and structure hash are prepended to serialised stream
instance (Binary a, HasStructuralInfo a, KnownNat v) => Binary (BinaryTagged v a) where
  put (BinaryTagged x) = put ver' >> put hash' >> put x
    where
      proxyV = Proxy :: Proxy v
      proxyA = Proxy :: Proxy a
      ver' = fromIntegral (natVal proxyV) :: Version
      hash' = structuralInfoSha1ByteStringDigest . structuralInfo $ proxyA

  get = do
      ver <- get
      if ver == ver'
         then do hash <- get
                 if hash == hash'
                    then fmap BinaryTagged get
                    else fail $ "Non matching structure hashes: got" <> show (Base16.encode hash) <> "; expected: " <> show (Base16.encode hash')
         else fail $ "Non matching versions: got " <> show ver <> "; expected: " <> show ver'
    where
      proxyV = Proxy :: Proxy v
      proxyA = Proxy :: Proxy a
      ver' = fromIntegral (natVal proxyV) :: Version
      hash' = bytestringDigest . structuralInfoSha1Digest . structuralInfo $ proxyA

-- | Data type structure, with (some) nominal information.
data StructuralInfo = NominalType String
                | NominalNewtype String StructuralInfo
                | StructuralInfo String [[StructuralInfo]]
  deriving (Eq, Ord, Show, GHC.Generic)

instance Binary StructuralInfo

-- | Type class providing `StructuralInfo` for each data type.
--
-- For regular non-recursive ADTs 'HasStructuralInfo' can be derived generically.
--
-- > data Record = Record { a :: Int, b :: Bool, c :: [Char] } deriving (Generic)
-- > instance hasStructuralInfo Record
--
-- For stable types, you can provide only type name
--
-- > instance HasStructuralInfo Int where structuralInfo = ghcNominalType -- infer name from Generic information
-- > instance HasStructuralInfo Integer where structuralInfo _ = NominalType "Integer"
--
-- Recursive type story is a bit sad atm. If the type structure is stable, you can do:
--
-- > instance HasStructuralInfo a => HasStructuralInfo [a] where structuralInfo = ghcStructuralInfo1
class HasStructuralInfo a where
  structuralInfo :: Proxy a -> StructuralInfo

  default structuralInfo :: ( GHC.Generic a
                            , All2 HasStructuralInfo (GCode a)
                            , GDatatypeInfo a
#if MIN_VERSION_generics_sop(0,2,0)
                            , SListI2 (GCode a)
#else
                            , SingI (GCode a)
#endif
                            ) => Proxy a -> StructuralInfo
  structuralInfo = ghcStructuralInfo

-- | A helper type family for 'encodeTaggedFile' and 'decodeTaggedFile'.
--
-- The default definition is @'SemanticVersion' a = 0@
class KnownNat (SemanticVersion a) => HasSemanticVersion (a :: *) where
  type SemanticVersion a :: Nat
  type SemanticVersion a = 0

instance HasStructuralInfo StructuralInfo
instance HasSemanticVersion StructuralInfo

structuralInfoSha1Digest :: StructuralInfo -> Digest SHA1State
structuralInfoSha1Digest = sha1 . encode

structuralInfoSha1ByteStringDigest :: StructuralInfo -> LBS.ByteString
structuralInfoSha1ByteStringDigest = bytestringDigest . structuralInfoSha1Digest

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

ghcStructuralInfo :: ( GHC.Generic a
                     , All2 HasStructuralInfo (GCode a)
                     , GDatatypeInfo a
#if MIN_VERSION_generics_sop(0,2,0)
                     , SListI2 (GCode a)
#else
                     , SingI (GCode a)
#endif
                     )
                  => Proxy a
                  -> StructuralInfo
ghcStructuralInfo proxy = sopStructuralInfoS (gdatatypeInfo proxy)

ghcNominalType ::  (GHC.Generic a,  GDatatypeInfo a) => Proxy a -> StructuralInfo
ghcNominalType proxy = sopNominalTypeS (gdatatypeInfo proxy)

ghcStructuralInfo1 :: forall f a. (GHC.Generic1 f, GDatatypeInfo (f a), HasStructuralInfo a) => Proxy (f a) -> StructuralInfo
ghcStructuralInfo1 proxy = sopStructuralInfo1S (structuralInfo (Proxy :: Proxy a)) (gdatatypeInfo proxy)

-- SOP derivation

sopStructuralInfo :: forall a. (Generic a, HasDatatypeInfo a, All2 HasStructuralInfo (Code a)) => Proxy a -> StructuralInfo
sopStructuralInfo proxy = sopStructuralInfoS (datatypeInfo proxy)

sopStructuralInfoS :: forall xss. ( All2 HasStructuralInfo xss
#if MIN_VERSION_generics_sop(0,2,0)
                                  , SListI2 xss
#else
                                  , SingI xss
#endif
                                  )
                   => DatatypeInfo xss
                   -> StructuralInfo
sopStructuralInfoS di@(Newtype _ _ ci)  = NominalNewtype (datatypeName di) (sopNominalNewtype ci)
sopStructuralInfoS di@(ADT _ _ _)       = StructuralInfo (datatypeName di) (sopNominalAdtPOP (hpure Proxy :: POP Proxy xss))

sopNominalNewtype :: forall x. HasStructuralInfo x => ConstructorInfo '[x] -> StructuralInfo
sopNominalNewtype _ = structuralInfo (Proxy :: Proxy x)

sopNominalAdtPOP :: (All2 HasStructuralInfo xss) => POP Proxy xss -> [[StructuralInfo]]
sopNominalAdtPOP (POP np2) = sopNominalAdt np2

sopNominalAdt :: (All2 HasStructuralInfo xss) => NP (NP Proxy) xss -> [[StructuralInfo]]
sopNominalAdt Nil          = []
sopNominalAdt (p :* ps)  = sopStructuralInfoP p : sopNominalAdt ps

sopStructuralInfoP :: (All HasStructuralInfo xs) => NP Proxy xs -> [StructuralInfo]
sopStructuralInfoP Nil = []
sopStructuralInfoP (proxy :* rest) =  structuralInfo proxy : sopStructuralInfoP rest

sopNominalType :: forall a. (Generic a, HasDatatypeInfo a) => Proxy a -> StructuralInfo
sopNominalType proxy = sopNominalTypeS (datatypeInfo proxy)

sopNominalTypeS :: DatatypeInfo xss -> StructuralInfo
sopNominalTypeS di = NominalType (datatypeName di)

sopStructuralInfo1 :: forall f a. (Generic (f a), HasDatatypeInfo (f a), HasStructuralInfo a) => Proxy (f a) -> StructuralInfo
sopStructuralInfo1 proxy = sopStructuralInfo1S (structuralInfo (Proxy :: Proxy a)) (datatypeInfo proxy)

sopStructuralInfo1S :: StructuralInfo -> DatatypeInfo xss -> StructuralInfo
sopStructuralInfo1S nsop di = NominalNewtype (datatypeName di) nsop

-------------------------------------------------------------------------------
-- SOP helpers
-------------------------------------------------------------------------------

#if !MIN_VERSION_generics_sop(0,2,3)
datatypeName :: DatatypeInfo xss -> DatatypeName
datatypeName (Newtype _ d _)  = d
datatypeName (ADT _ d _)      = d
#endif

-- | Interleaving
--
-- > 3 | 9  .  .  .  .
-- > 2 | 5  8  .  .  .
-- > 1 | 2  4  7 11  .
-- > 0 | 0  1  3  6 10
-- > -----------------
-- >     0  1  2  3  4
--
-- This can be calculated by @f x y = sum ([0..x+y]) + y@
type Interleave (n :: Nat) (m :: Nat) = SumUpTo (n + m) + m
type SumUpTo (n :: Nat) = Div2 (n GHC.TypeLits.* (n + 1))
type family Div2 (n :: Nat) :: Nat where
  Div2 0 = 0
  Div2 1 = 0
  Div2 n = 1 + Div2 (n - 2)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance HasStructuralInfo Bool where structuralInfo = ghcNominalType
instance HasStructuralInfo Char where structuralInfo _ = NominalType "Char"
instance HasStructuralInfo Int where structuralInfo _ = NominalType "Int"
instance HasStructuralInfo Word where structuralInfo _ = NominalType "Word"
instance HasStructuralInfo Integer where structuralInfo _ = NominalType "Integer"

instance HasStructuralInfo Int8 where structuralInfo _ = NominalType "Int8"
instance HasStructuralInfo Int16 where structuralInfo _ = NominalType "Int16"
instance HasStructuralInfo Int32 where structuralInfo _ = NominalType "Int32"
instance HasStructuralInfo Int64 where structuralInfo _ = NominalType "Int64"

instance HasStructuralInfo Word8 where structuralInfo _ = NominalType "Word8"
instance HasStructuralInfo Word16 where structuralInfo _ = NominalType "Word16"
instance HasStructuralInfo Word32 where structuralInfo _ = NominalType "Word32"
instance HasStructuralInfo Word64 where structuralInfo _ = NominalType "Word64"

instance HasSemanticVersion Bool
instance HasSemanticVersion Char
instance HasSemanticVersion Int
instance HasSemanticVersion Word
instance HasSemanticVersion Integer

instance HasSemanticVersion Int8
instance HasSemanticVersion Int16
instance HasSemanticVersion Int32
instance HasSemanticVersion Int64

instance HasSemanticVersion Word8
instance HasSemanticVersion Word16
instance HasSemanticVersion Word32
instance HasSemanticVersion Word64

-- | /Since binary-tagged-0.1.3.0/
instance HasStructuralInfo Ordering where structuralInfo = ghcNominalType

-- | /Since binary-tagged-0.1.3.0/
instance HasSemanticVersion Ordering

-- | /Since binary-tagged-0.1.3.0/
instance HasStructuralInfo Float where structuralInfo _ = NominalType "Float"

-- | /Since binary-tagged-0.1.3.0/
instance HasStructuralInfo Double where structuralInfo _ = NominalType "Double"

-- | /Since binary-tagged-0.1.3.0/
instance HasSemanticVersion Float

-- | /Since binary-tagged-0.1.3.0/
instance HasSemanticVersion Double

-------------------------------------------------------------------------------
-- Recursive types: List, NonEmpty
-------------------------------------------------------------------------------

instance HasStructuralInfo a => HasStructuralInfo [a] where structuralInfo = ghcStructuralInfo1
instance HasSemanticVersion a => HasSemanticVersion [a] where
  type SemanticVersion [a] = SemanticVersion a

instance HasStructuralInfo a => HasStructuralInfo (NE.NonEmpty a) where structuralInfo = ghcStructuralInfo1
instance HasSemanticVersion a => HasSemanticVersion (NE.NonEmpty a) where
  type SemanticVersion (NE.NonEmpty a) = SemanticVersion a

-------------------------------------------------------------------------------
-- Basic types
-------------------------------------------------------------------------------

instance HasStructuralInfo a => HasStructuralInfo (Maybe a)
instance HasSemanticVersion a => HasSemanticVersion (Maybe a) where
  type SemanticVersion (Maybe a) = SemanticVersion a

instance HasStructuralInfo a => HasStructuralInfo (Ratio.Ratio a) where
  structuralInfo _ = NominalNewtype "Ratio" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (Ratio.Ratio a) where
  type SemanticVersion (Ratio.Ratio a) = SemanticVersion a

instance (HasStructuralInfo a, HasStructuralInfo b) => HasStructuralInfo (Either a b)
instance (HasSemanticVersion a, HasSemanticVersion b, KnownNat (SemanticVersion (Either a b))) => HasSemanticVersion (Either a b) where
  type SemanticVersion (Either a b) = Interleave (SemanticVersion a) (SemanticVersion b)

-------------------------------------------------------------------------------
-- tuples
-------------------------------------------------------------------------------

instance (HasStructuralInfo a, HasStructuralInfo b) => HasStructuralInfo (a, b)
instance (HasStructuralInfo a, HasStructuralInfo b, HasStructuralInfo c) => HasStructuralInfo (a, b, c)
instance (HasStructuralInfo a, HasStructuralInfo b, HasStructuralInfo c, HasStructuralInfo d) => HasStructuralInfo (a, b, c, d)

instance (HasSemanticVersion a
         ,HasSemanticVersion b
         ,KnownNat (SemanticVersion (a, b))) => HasSemanticVersion (a, b) where
  type SemanticVersion (a, b) = Interleave (SemanticVersion a) (SemanticVersion b)

-- | /Since binary-tagged-0.1.3.0/
instance (HasSemanticVersion a
         ,HasSemanticVersion b
         ,HasSemanticVersion c
         ,KnownNat (SemanticVersion (a, b, c))) => HasSemanticVersion (a, b, c) where
  type SemanticVersion (a, b, c) = Interleave (SemanticVersion a) (SemanticVersion (b, c))

-- | /Since binary-tagged-0.1.3.0/
instance (HasSemanticVersion a
         ,HasSemanticVersion b
         ,HasSemanticVersion c
         ,HasSemanticVersion d
         ,KnownNat (SemanticVersion (a, b, c, d))) => HasSemanticVersion (a, b, c, d) where
  type SemanticVersion (a, b, c, d) = Interleave (SemanticVersion a) (SemanticVersion (b, c, d))

-------------------------------------------------------------------------------
-- Unit
-------------------------------------------------------------------------------

-- | /Since binary-tagged-0.1.3.0/
instance HasStructuralInfo () where structuralInfo _ = NominalType "()"

-- | /Since binary-tagged-0.1.3.0/
instance HasSemanticVersion ()

-------------------------------------------------------------------------------
-- Data.Fixed
-------------------------------------------------------------------------------

-- | /Since binary-tagged-0.1.3.0/
instance HasStructuralInfo a => HasStructuralInfo (Fixed.Fixed a) where
  structuralInfo _ = StructuralInfo "Fixed" [[ structuralInfo (Proxy :: Proxy a) ]]

instance HasStructuralInfo Fixed.E0 where structuralInfo _ = NominalType "E0"
instance HasStructuralInfo Fixed.E1 where structuralInfo _ = NominalType "E1"
instance HasStructuralInfo Fixed.E2 where structuralInfo _ = NominalType "E2"
instance HasStructuralInfo Fixed.E3 where structuralInfo _ = NominalType "E3"
instance HasStructuralInfo Fixed.E6 where structuralInfo _ = NominalType "E6"
instance HasStructuralInfo Fixed.E9 where structuralInfo _ = NominalType "E9"
instance HasStructuralInfo Fixed.E12 where structuralInfo _ = NominalType "E12"

-- | /Since binary-tagged-0.1.3.0/
instance HasSemanticVersion (Fixed.Fixed a)

-------------------------------------------------------------------------------
-- Data.Version
-------------------------------------------------------------------------------

-- | /Since binary-tagged-0.1.3.0/
instance HasStructuralInfo Version.Version where
  structuralInfo _ = StructuralInfo "Version" [[ structuralInfo (Proxy :: Proxy [Int])
                                               , structuralInfo (Proxy :: Proxy [String])
                                              ]]
-- Version has no Generic instance :(

-- | /Since binary-tagged-0.1.3.0/
instance HasSemanticVersion Version.Version

-------------------------------------------------------------------------------
-- Data.Monoid
-------------------------------------------------------------------------------

instance HasStructuralInfo a => HasStructuralInfo (Monoid.Sum a)
instance HasSemanticVersion a => HasSemanticVersion (Monoid.Sum a) where
  type SemanticVersion (Monoid.Sum a) = SemanticVersion a

instance HasStructuralInfo a => HasStructuralInfo (Monoid.Product a)
instance HasSemanticVersion a => HasSemanticVersion (Monoid.Product a) where
  type SemanticVersion (Monoid.Product a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Monoid.Dual a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Monoid.Dual a) where
  type SemanticVersion (Monoid.Dual a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Monoid.First a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Monoid.First a) where
  type SemanticVersion (Monoid.First a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Monoid.Last a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Monoid.Last a) where
  type SemanticVersion (Monoid.Last a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo Monoid.All
-- | /Since binary-tagged-0.1.4.0/
instance  HasSemanticVersion Monoid.All

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo Monoid.Any
-- | /Since binary-tagged-0.1.4.0/
instance  HasSemanticVersion Monoid.Any

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Semigroup.Min a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Semigroup.Min a) where
  type SemanticVersion (Semigroup.Min a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Semigroup.Max a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Semigroup.Max a) where
  type SemanticVersion (Semigroup.Max a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Semigroup.First a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Semigroup.First a) where
  type SemanticVersion (Semigroup.First a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Semigroup.Last a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Semigroup.Last a) where
  type SemanticVersion (Semigroup.Last a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Semigroup.WrappedMonoid a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Semigroup.WrappedMonoid a) where
  type SemanticVersion (Semigroup.WrappedMonoid a) = SemanticVersion a

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo a => HasStructuralInfo (Semigroup.Option a)
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion a => HasSemanticVersion (Semigroup.Option a) where
  type SemanticVersion (Semigroup.Option a) = SemanticVersion a

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

instance HasStructuralInfo BS.ByteString where structuralInfo _ = NominalType "ByteString.Strict"
instance HasStructuralInfo LBS.ByteString where structuralInfo _ = NominalType "ByteString.Lazy"

instance HasSemanticVersion BS.ByteString
instance HasSemanticVersion LBS.ByteString

-------------------------------------------------------------------------------
-- nats
-------------------------------------------------------------------------------

-- | /Since binary-tagged-0.1.4.0/
instance HasStructuralInfo Natural.Natural where structuralInfo _ = NominalType "Numeric.Natural"
-- | /Since binary-tagged-0.1.4.0/
instance HasSemanticVersion Natural.Natural

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance HasStructuralInfo S.Text where structuralInfo _ = NominalType "Text.Strict"
instance HasStructuralInfo L.Text where structuralInfo _ = NominalType "Text.Lazy"

instance HasSemanticVersion S.Text
instance HasSemanticVersion L.Text

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance HasStructuralInfo a => HasStructuralInfo (IntMap.IntMap a) where
  structuralInfo _ = NominalNewtype "IntMap" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (IntMap.IntMap a) where
  type SemanticVersion (IntMap.IntMap a) = SemanticVersion a

instance HasStructuralInfo IntSet.IntSet where
  structuralInfo _ = NominalType "IntSet"
instance HasSemanticVersion IntSet.IntSet

instance (HasStructuralInfo k, HasStructuralInfo v) => HasStructuralInfo (Map.Map k v) where
  structuralInfo _ = StructuralInfo "Map" [[ structuralInfo (Proxy :: Proxy k), structuralInfo (Proxy :: Proxy v) ]]
instance (HasSemanticVersion k, HasSemanticVersion v, KnownNat (SemanticVersion (Map.Map k v))) => HasSemanticVersion (Map.Map k v) where
  type SemanticVersion (Map.Map k v) = Interleave (SemanticVersion k) (SemanticVersion v)

instance HasStructuralInfo a => HasStructuralInfo (Seq.Seq a) where
  structuralInfo _ = NominalNewtype "Seq" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (Seq.Seq a) where
  type SemanticVersion (Seq.Seq a) = SemanticVersion a

instance HasStructuralInfo a => HasStructuralInfo (Set.Set a) where
  structuralInfo _ = NominalNewtype "Set" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (Set.Set a) where
  type SemanticVersion (Set.Set a) = SemanticVersion a

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (HasStructuralInfo k, HasStructuralInfo v) => HasStructuralInfo (HML.HashMap k v) where
  structuralInfo _ = StructuralInfo "HashMap" [[ structuralInfo (Proxy :: Proxy k), structuralInfo (Proxy :: Proxy v) ]]
instance (HasSemanticVersion k, HasSemanticVersion v, KnownNat (SemanticVersion (HML.HashMap k v))) => HasSemanticVersion (HML.HashMap k v) where
  type SemanticVersion (HML.HashMap k v) = Interleave (SemanticVersion k) (SemanticVersion v)

instance HasStructuralInfo a => HasStructuralInfo (HS.HashSet a) where
  structuralInfo _ = NominalNewtype "HashSet" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (HS.HashSet a) where
  type SemanticVersion (HS.HashSet a) = SemanticVersion a

-------------------------------------------------------------------------------
-- array
-------------------------------------------------------------------------------

instance (HasStructuralInfo i, HasStructuralInfo e) => HasStructuralInfo (Array.Array i e) where
  structuralInfo _ = StructuralInfo "Array" [[ structuralInfo (Proxy :: Proxy i), structuralInfo (Proxy :: Proxy e) ]]
instance (HasSemanticVersion i, HasSemanticVersion e, KnownNat (SemanticVersion (Array.Array i e))) => HasSemanticVersion (Array.Array i e) where
  type SemanticVersion (Array.Array i e) = Interleave (SemanticVersion i) (SemanticVersion e)

instance (HasStructuralInfo i, HasStructuralInfo e) => HasStructuralInfo (Array.UArray i e) where
  structuralInfo _ = StructuralInfo "UArray" [[ structuralInfo (Proxy :: Proxy i), structuralInfo (Proxy :: Proxy e) ]]
instance (HasSemanticVersion i, HasSemanticVersion e, KnownNat (SemanticVersion (Array.UArray i e))) => HasSemanticVersion (Array.UArray i e) where
  type SemanticVersion (Array.UArray i e) = Interleave (SemanticVersion i) (SemanticVersion e)

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance HasStructuralInfo a => HasStructuralInfo (V.Vector a) where
  structuralInfo _ = NominalNewtype "Vector" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (V.Vector a) where
  type SemanticVersion (V.Vector a) = SemanticVersion a

instance HasStructuralInfo a => HasStructuralInfo (U.Vector a) where
  structuralInfo _ = NominalNewtype "Vector.Unboxed" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (U.Vector a) where
  type SemanticVersion (U.Vector a) = SemanticVersion a

instance HasStructuralInfo a => HasStructuralInfo (S.Vector a) where
  structuralInfo _ = NominalNewtype "Vector.Storable" $ structuralInfo (Proxy :: Proxy a)
instance HasSemanticVersion a => HasSemanticVersion (S.Vector a) where
  type SemanticVersion (S.Vector a) = SemanticVersion a

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance HasStructuralInfo Time.UTCTime where structuralInfo _ = NominalType "UTCTime"
instance HasStructuralInfo Time.DiffTime where structuralInfo _ = NominalType "DiffTime"
instance HasStructuralInfo Time.UniversalTime where structuralInfo _ = NominalType "UniversalTime"
instance HasStructuralInfo Time.NominalDiffTime where structuralInfo _ = NominalType "NominalDiffTime"
instance HasStructuralInfo Time.Day where structuralInfo _ = NominalType "Day"
instance HasStructuralInfo Time.TimeZone where structuralInfo _ = NominalType "TimeZone"
instance HasStructuralInfo Time.TimeOfDay where structuralInfo _ = NominalType "TimeOfDay"
instance HasStructuralInfo Time.LocalTime where structuralInfo _ = NominalType "LocalTime"

instance HasSemanticVersion Time.UTCTime
instance HasSemanticVersion Time.DiffTime
instance HasSemanticVersion Time.UniversalTime
instance HasSemanticVersion Time.NominalDiffTime
instance HasSemanticVersion Time.Day
instance HasSemanticVersion Time.TimeZone
instance HasSemanticVersion Time.TimeOfDay
instance HasSemanticVersion Time.LocalTime

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_aeson

-- TODO: derive sop
instance HasStructuralInfo Aeson.Value where structuralInfo _ = NominalType "Aeson.Value"
instance HasSemanticVersion Aeson.Value
#endif
