-- |
--
-- Copyright: (c) 2019-2020 Oleg Grenrus
--
-- Structurally tag data.
--
-- See @binary-tagged@ package taking use of this.
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
-- Technically, 'Structured' is not related to 'Binary', and may
-- be useful in other uses.
--
module Data.Structured (
    -- * Structured class
    Structured (structure),
    structureHash,
    structureBuilder,
    genericStructure,
    GStructured,
    nominalStructure,
    containerStructure,
    -- * MD5
    MD5,
    showMD5,
    md5,
    md5FromInteger,
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

import Data.Structured.Internal
import Data.Structured.MD5
