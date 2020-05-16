{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Tagged
-- Copyright   :  (C) 2015-2020 Oleg Grenrus
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
module Data.Binary.Tagged (
    -- * Encoding and decoding
    -- | These functions operate like @binary@'s counterparts,
    -- but the serialised version has a structure hash in front.
    structuredEncode,
    structuredEncodeFile,
    structuredDecode,
    structuredDecodeOrFailIO,
    structuredDecodeFileOrFail,
    -- * Structured class
    Structured (structure),
    structureHash,
    structureBuilder,
    genericStructure,
    GStructured,
    nominalStructure,
    containerStructure,
    -- * Structure type
    Structure (..),
    TypeName,
    ConstructorName,
    TypeVersion,
    SopStructure,
    hashStructure,
    typeVersion,
    typeName,
    -- * MD5
    MD5,
    showMD5,
    md5,
    md5FromInteger,
    binaryPutMD5,
    binaryGetMD5,
) where

import Data.Structured
import Data.Structured.Internal

import qualified Data.Binary          as Binary
import qualified Data.Binary.Get      as Binary
import qualified Data.Binary.Put      as Binary
import qualified Data.ByteString.Lazy as LBS

import Control.Exception (ErrorCall (..), catch, evaluate)
import Data.Tagged       (Tagged (..), untag)
import GHC.Fingerprint   (Fingerprint (..))

-------------------------------------------------------------------------------
-- Helper data
-------------------------------------------------------------------------------

data Tag a = Tag

instance Structured a => Binary.Binary (Tag a) where
    get = do
        actual <- binaryGetMD5
        if actual == expected
        then return Tag
        else fail $ concat
            [ "Non-matching structured hashes: "
            , showMD5 actual
            , "; expected: "
            , showMD5 expected
            ]
      where
        expected = untag (structureHash' :: Tagged a MD5)

    put _ = binaryPutMD5 expected
      where
        expected = untag (structureHash' :: Tagged a MD5)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Structured 'Binary.encode'.
-- Encode a value to using binary serialisation to a lazy 'LBS.ByteString'.
-- Encoding starts with 16 byte large structure hash.
structuredEncode
  :: forall a. (Binary.Binary a, Structured a)
  => a -> LBS.ByteString
structuredEncode x = Binary.encode (Tag :: Tag a, x)

-- | Lazily serialise a value to a file
structuredEncodeFile :: (Binary.Binary a, Structured a) => FilePath -> a -> IO ()
structuredEncodeFile f = LBS.writeFile f . structuredEncode

-- | Structured 'Binary.decode'.
-- Decode a value from a lazy 'LBS.ByteString', reconstructing the original structure.
-- Throws pure exception on invalid inputs.
structuredDecode
  :: forall a. (Binary.Binary a, Structured a)
  => LBS.ByteString -> a
structuredDecode lbs = snd (Binary.decode lbs :: (Tag a, a))

structuredDecodeOrFailIO :: (Binary.Binary a, Structured a) => LBS.ByteString -> IO (Either String a)
structuredDecodeOrFailIO bs =
    catch (evaluate (structuredDecode bs) >>= return . Right) handler
  where
#if MIN_VERSION_base(4,9,0)
    handler (ErrorCallWithLocation str _) = return $ Left str
#else
    handler (ErrorCall str) = return $ Left str
#endif

-- | Lazily reconstruct a value previously written to a file.
structuredDecodeFileOrFail :: (Binary.Binary a, Structured a) => FilePath -> IO (Either String a)
structuredDecodeFileOrFail f = structuredDecodeOrFailIO =<< LBS.readFile f

-------------------------------------------------------------------------------
-- MD5 extras
-------------------------------------------------------------------------------

binaryPutMD5 :: MD5 -> Binary.Put
binaryPutMD5 (Fingerprint a b) = do
    Binary.putWord64le a
    Binary.putWord64le b

binaryGetMD5 :: Binary.Get MD5
binaryGetMD5 = do
    a <- Binary.getWord64le
    b <- Binary.getWord64le
    return (Fingerprint a b)
