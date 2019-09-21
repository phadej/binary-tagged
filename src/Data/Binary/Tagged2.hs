{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
--
-- Copyright: (c) 2019 Oleg Grenrus
-- License: GPL-3.0-or-later
--
module Data.Binary.Tagged2 (
    -- * Functions
    structuredEncode,
    structuredDecode,
    structuredDecodeOrFail,
    -- * Classes
    Structured (..),
    GStructured,
    nominalStructure,
    containerStructure,
    containerStructure2,
    -- * Types
    Structure (..),
    TypeName,
    ConstructorName,
    TypeVersion,
    SopStructure,
    succVersion,
    ) where

import Data.Int           (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy         (Proxy (..))
import Data.Ratio         (Ratio)
import Data.Typeable      (Typeable, typeRep)
import Data.Void          (Void)
import Data.Word          (Word16, Word32, Word64, Word8, Word)
import Numeric.Natural    (Natural)

import GHC.Generics

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.IntMap          as IM
import qualified Data.IntSet          as IS
import qualified Data.Map             as Map
import qualified Data.Monoid          as Mon
import qualified Data.Semigroup       as Semi
import qualified Data.Sequence        as Seq
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.ByteString.Base16  as Base16
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as UV
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import qualified Crypto.Hash.SHA1        as SHA1

-- TODO: time

-- TODO: aeson

-- TODO: CPP
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
type TypeVersion     = Word32

data Structure
    = Nominal   !TypeVersion TypeName [Structure]
    | Newtype   !TypeVersion TypeName Structure
    | Structure !TypeVersion TypeName SopStructure
  deriving (Eq, Ord, Show, Generic)

instance Binary.Binary Structure
instance Structured Structure

type SopStructure = [(ConstructorName, [Structure])]

succVersion :: Structure -> Structure
succVersion (Nominal v n s)   = Nominal (succ v) n s
succVersion (Newtype v n s)   = Newtype (succ v) n s
succVersion (Structure v n s) = Structure (succ v) n s

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class Structured a where
    structure :: Proxy a -> Structure
    default structure :: (Generic a, GStructured (Rep a)) => Proxy a -> Structure
    structure _ = gstructured (Proxy :: Proxy (Rep a)) 0

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

structuredEncode
  :: forall a. (Binary.Binary a, Structured a)
  => a -> LBS.ByteString
structuredEncode x = Binary.encode (Tag :: Tag a, x)

structuredDecode
  :: forall a. (Binary.Binary a, Structured a)
  => LBS.ByteString -> a
structuredDecode lbs = snd (Binary.decode lbs :: (Tag a, a))

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
        expected = SHA1.hashlazy (Binary.encode (structure (Proxy :: Proxy a)))

    put _ = Binary.putByteString
          $ SHA1.hashlazy (Binary.encode (structure (Proxy :: Proxy a)))

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

-- TODO: time instances
