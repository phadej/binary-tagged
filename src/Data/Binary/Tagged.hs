{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
--
-- Copyright: (c) 2019 Oleg Grenrus
-- License: GPL-2.0-or-later
--
-- Structurally tag binary serialisaton stream.
-- Useful when most 'Binary' instances are 'Generic' derived.
--
-- Say you have a data type
--
-- @
-- data Record = Record
--   { _recordFields  :: HM.HashMap Text (Integer, ByteString)
--   , _recordEnabled :: Bool
--   }
--   deriving (Eq, Show, Generic)
--
-- instance 'Binary' Record
-- instance 'Structured' Record
-- @
--
-- then you can serialise and deserialise @Record@ values with a structure tag by simply
--
-- @
-- 'structuredEncode' record :: 'LBS.ByteString'
-- 'structuredDecode' lbs :: IO Record
-- @
--
-- If structure of @Record@ changes in between, deserialisation will fail early.
--
module Data.Binary.Tagged (
    -- * Encoding and decoding
    -- | These functions operate like @binary@'s counterparts,
    -- but the serialised version has a structure hash in front.
    structuredEncode,
    structuredDecode,
    structuredDecodeOrFail,
    -- * Structured class
    Structured (structure),
    Hash,
    structureHash,
    genericStructure,
    GStructured,
    nominalStructure,
    containerStructure,
    containerStructure2,
    -- * Structure type
    Structure (..),
    TypeName,
    ConstructorName,
    TypeVersion,
    SopStructure,
    hashStructure,
    typeVersion,
    typeName,
    ) where

import Data.Int           (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy         (Proxy (..))
import Data.Ratio         (Ratio)
import Data.Tagged        (Tagged (..), untag)
import Data.Typeable      (Typeable, typeRep)
import Data.Void          (Void)
import Data.Word          (Word, Word16, Word32, Word64, Word8)
import Numeric.Natural    (Natural)

import GHC.Generics

import qualified Crypto.Hash.SHA1       as SHA1
import qualified Data.Binary            as Binary
import qualified Data.Binary.Get        as Binary
import qualified Data.Binary.Put        as Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Data.IntMap            as IM
import qualified Data.IntSet            as IS
import qualified Data.Map               as Map
import qualified Data.Monoid            as Mon
import qualified Data.Scientific        as Sci
import qualified Data.Semigroup         as Semi
import qualified Data.Sequence          as Seq
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Time.Compat       as Time
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as SV
import qualified Data.Vector.Unboxed    as UV

#ifdef MIN_VERSION_aeson
import qualified Data.Aeson as Aeson
#endif

#if __GLASGOW_HASKELL__ >= 800
import Data.Kind (Type)
#else
#define Type *
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TypeName        = String
type ConstructorName = String

-- | A sematic version of a data type. Usually 0.
type TypeVersion     = Word32

-- | Structure of a datatype
data Structure
    = Nominal   !TypeVersion TypeName [Structure]  -- ^ nominal, yet can be parametrised by other structures.
    | Newtype   !TypeVersion TypeName Structure    -- ^ a newtype wrapper
    | Structure !TypeVersion TypeName SopStructure -- ^ sum-of-products structure
  deriving (Eq, Ord, Show, Generic)

instance Binary.Binary Structure
instance Structured Structure

type SopStructure = [(ConstructorName, [Structure])]

-- | A hash digest of 'Structure'. A 20 bytes strict 'BS.ByteString'.
hashStructure :: Structure -> BS.ByteString
hashStructure = SHA1.hashlazy . Binary.encode

-- | A van-Laarhoven lens into 'TypeVersion' of 'Structure'
--
-- @
-- 'typeVersion' :: Lens' 'Structure' 'TypeVersion'
-- @
typeVersion :: Functor f => (TypeVersion -> f TypeVersion) -> Structure -> f Structure
typeVersion f (Nominal v n s)   = fmap (\v' -> Nominal v' n s) (f v)
typeVersion f (Newtype v n s)   = fmap (\v' -> Newtype v' n s) (f v)
typeVersion f (Structure v n s) = fmap (\v' -> Structure v' n s) (f v)

-- | A van-Laarhoven lens into 'TypeName' of 'Structure'
--
-- @
-- 'typeName' :: Lens' 'Structure' 'TypeName'
-- @
typeName :: Functor f => (TypeName -> f TypeName) -> Structure -> f Structure
typeName f (Nominal v n s)   = fmap (\n' -> Nominal v n' s) (f n)
typeName f (Newtype v n s)   = fmap (\n' -> Newtype v n' s) (f n)
typeName f (Structure v n s) = fmap (\n' -> Structure v n' s) (f n)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Class of types with a known 'Structure'.
--
-- For regular non-recursive data types 'Structured' can be derived generically.
--
-- @
-- data Record = Record { a :: Int, b :: Bool, c :: [Char] } deriving ('Generic')
-- instance 'Structured' Record
-- @
--
-- For other types check 'nominalStructure', 'containerStructure' etc.
--
class Structured a where
    structure :: Proxy a -> Structure
    default structure :: (Generic a, GStructured (Rep a)) => Proxy a -> Structure
    structure = genericStructure

    -- This member is hidden. It's there to precalc
    structureHash' :: Tagged a Hash
    structureHash' = Tagged (hashStructure (structure (Proxy :: Proxy a)))

-- | Hash (of Structure) is 20 bytes long strict 'BS.ByteString'.
type Hash = BS.ByteString

-- | Semantically @'hashStructure' . 'structure'@.
structureHash :: forall a. Structured a => Proxy a -> Hash
structureHash _ = untag (structureHash' :: Tagged a Hash)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Structured 'Binary.encode'.
-- Encode a value to using binary serialisation to a lazy 'LBS.ByteString'.
-- Encoding starts with 20 byte large structure hash.
structuredEncode
  :: forall a. (Binary.Binary a, Structured a)
  => a -> LBS.ByteString
structuredEncode x = Binary.encode (Tag :: Tag a, x)

-- | Structured 'Binary.decode'.
-- Decode a value from a lazy 'LBS.ByteString', reconstructing the original structure.
-- Throws pure exception on invalid inputs.
structuredDecode
  :: forall a. (Binary.Binary a, Structured a)
  => LBS.ByteString -> a
structuredDecode lbs = snd (Binary.decode lbs :: (Tag a, a))

-- | Structured 'Binary.decodeOrFail'.
structuredDecodeOrFail
  :: forall a. (Binary.Binary a, Structured a)
  => LBS.ByteString
  -> Either (LBS.ByteString, Binary.ByteOffset, String) (LBS.ByteString, Binary.ByteOffset, a)
structuredDecodeOrFail lbs = case Binary.decodeOrFail lbs of
    Left err                          -> Left err
    Right (bs, bo, (Tag :: Tag a, x)) -> Right (bs, bo, x)

data Tag a = Tag

instance Structured a => Binary.Binary (Tag a) where
    get = do
        actual <- Binary.getByteString 20
        if actual == expected
        then return Tag
        else fail $ concat
            [ "Non-matching structured hashes: "
            , show (Base16.encode actual)
            , "; expected: "
            , show (Base16.encode expected)
            ]
      where
        expected = untag (structureHash' :: Tagged a BS.ByteString)

    put _ = Binary.putByteString expected
      where
        expected = untag (structureHash' :: Tagged a BS.ByteString)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

-- | Use 'Typeable' to infer name
nominalStructure :: Typeable a => Proxy a -> Structure
nominalStructure p = Nominal 0 (show (typeRep p)) []

containerStructure :: forall f a. (Typeable f, Structured a) => Proxy (f a) -> Structure
containerStructure _ = Nominal 0 (show (typeRep (Proxy :: Proxy f)))
    [ structure (Proxy :: Proxy a)
    ]

containerStructure2 :: forall f a b. (Typeable f, Structured a, Structured b) => Proxy (f a b) -> Structure
containerStructure2 _ = Nominal 0 (show (typeRep (Proxy :: Proxy f)))
    [ structure (Proxy :: Proxy a)
    , structure (Proxy :: Proxy b)
    ]

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

-- | Derive 'structure' genrically.
genericStructure :: forall a. (Generic a, GStructured (Rep a)) => Proxy a -> Structure
genericStructure _ = gstructured (Proxy :: Proxy (Rep a)) 0

-- | Used to implement 'genericStructure'.
class GStructured (f :: Type -> Type) where
    gstructured :: Proxy f -> TypeVersion -> Structure

instance (i ~ D, Datatype c, GStructuredSum f) => GStructured (M1 i c f) where
    gstructured _ v = case sop of
        [(_, [s])] | isNewtype p -> Newtype v name s
        _                        -> Structure v name sop
      where
        p    = undefined :: M1 i c f ()
        name = datatypeName p
        sop  = gstructuredSum (Proxy :: Proxy f) []

class GStructuredSum (f :: Type -> Type) where
    gstructuredSum :: Proxy f -> SopStructure -> SopStructure

instance (i ~ C, Constructor c, GStructuredProd f) => GStructuredSum (M1 i c f) where
    gstructuredSum _ xs = (name, prod) : xs
      where
        name = conName (undefined :: M1 i c f ())
        prod = gstructuredProd (Proxy :: Proxy f) []

instance (GStructuredSum f, GStructuredSum g) => GStructuredSum (f :+: g) where
    gstructuredSum _ xs
        = gstructuredSum (Proxy :: Proxy f)
        $ gstructuredSum (Proxy :: Proxy g) xs

instance GStructuredSum V1 where
    gstructuredSum _ = id

class GStructuredProd (f :: Type -> Type) where
    gstructuredProd :: Proxy f -> [Structure] -> [Structure]

instance (i ~ s, GStructuredProd f) => GStructuredProd (M1 i c f) where
    gstructuredProd _ = gstructuredProd (Proxy :: Proxy f)

instance Structured c => GStructuredProd (K1 i c) where
    gstructuredProd _ xs = structure (Proxy :: Proxy c) : xs

instance GStructuredProd U1 where
    gstructuredProd _ = id

instance (GStructuredProd f, GStructuredProd g) => GStructuredProd (f :*: g) where
    gstructuredProd _ xs
        = gstructuredProd (Proxy :: Proxy f)
        $ gstructuredProd (Proxy :: Proxy g) xs

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Structured Void
instance Structured ()
instance Structured Bool
instance Structured Ordering

instance Structured Char    where structure = nominalStructure
instance Structured Int     where structure = nominalStructure
instance Structured Integer where structure = nominalStructure
instance Structured Natural where structure = nominalStructure

instance Structured Data.Word.Word where structure = nominalStructure

instance Structured Int8  where structure = nominalStructure
instance Structured Int16 where structure = nominalStructure
instance Structured Int32 where structure = nominalStructure
instance Structured Int64 where structure = nominalStructure

instance Structured Word8  where structure = nominalStructure
instance Structured Word16 where structure = nominalStructure
instance Structured Word32 where structure = nominalStructure
instance Structured Word64 where structure = nominalStructure

instance Structured Float  where structure = nominalStructure
instance Structured Double where structure = nominalStructure

instance Structured a => Structured (Maybe a)
instance (Structured a, Structured b) => Structured (Either a b)
instance Structured a => Structured (Ratio a) where structure = containerStructure
instance Structured a => Structured [a] where structure = containerStructure
instance Structured a => Structured (NonEmpty a) where structure = containerStructure

instance Structured a => Structured (Mon.Sum a)
instance Structured a => Structured (Mon.Product a)
instance Structured a => Structured (Mon.Dual a)
instance Structured a => Structured (Mon.First a)
instance Structured a => Structured (Mon.Last a)
instance Structured Mon.All
instance Structured Mon.Any

instance Structured a => Structured (Semi.Min a)
instance Structured a => Structured (Semi.Max a)
instance Structured a => Structured (Semi.First a)
instance Structured a => Structured (Semi.Last a)
instance Structured a => Structured (Semi.WrappedMonoid a)
instance Structured a => Structured (Semi.Option a)

instance (Structured a1, Structured a2) => Structured (a1, a2)
instance (Structured a1, Structured a2, Structured a3) => Structured (a1, a2, a3)
instance (Structured a1, Structured a2, Structured a3, Structured a4) => Structured (a1, a2, a3, a4)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5) => Structured (a1, a2, a3, a4, a5)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5, Structured a6) => Structured (a1, a2, a3, a4, a5, a6)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5, Structured a6, Structured a7) => Structured (a1, a2, a3, a4, a5, a6, a7)

-- TODO: Fixed instance

instance Structured BS.ByteString where structure = nominalStructure
instance Structured LBS.ByteString where structure = nominalStructure

instance Structured T.Text where structure = nominalStructure
instance Structured LT.Text where structure = nominalStructure

instance (Structured k, Structured v) => Structured (Map.Map k v) where structure = containerStructure2
instance (Structured k) => Structured (Set.Set k) where structure = containerStructure
instance (Structured v) => Structured (IM.IntMap v) where structure = containerStructure
instance Structured IS.IntSet where structure = nominalStructure
instance (Structured v) => Structured (Seq.Seq v) where structure = containerStructure

instance (Structured k, Structured v) => Structured (HM.HashMap k v) where structure = containerStructure2
instance (Structured k) => Structured (HS.HashSet k) where structure = containerStructure

instance Structured a => Structured (V.Vector a)  where structure = containerStructure
instance Structured a => Structured (UV.Vector a) where structure = containerStructure
instance Structured a => Structured (SV.Vector a) where structure = containerStructure

instance Structured Sci.Scientific where structure = nominalStructure

#ifdef MIN_VERSION_aeson
instance Structured Aeson.Value
#endif

instance Structured Time.UTCTime         where structure = nominalStructure
instance Structured Time.DiffTime        where structure = nominalStructure
instance Structured Time.UniversalTime   where structure = nominalStructure
instance Structured Time.NominalDiffTime where structure = nominalStructure
instance Structured Time.Day             where structure = nominalStructure
instance Structured Time.TimeZone        where structure = nominalStructure
instance Structured Time.TimeOfDay       where structure = nominalStructure
instance Structured Time.LocalTime       where structure = nominalStructure
instance Structured Time.DayOfWeek       where structure = nominalStructure

instance Structured (Proxy a)
instance Structured a => Structured (Tagged b a)

-- TODO: array instances
