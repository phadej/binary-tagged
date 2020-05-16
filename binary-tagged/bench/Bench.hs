{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Main (main) where

import Control.DeepSeq       (NFData)
import Criterion.Main        (bench, bgroup, defaultMain, nf)
import Data.Binary           (Binary, decode, encode)
import Data.Binary.Instances ()
import Data.Typeable         (Typeable)
import GHC.Generics          (Generic)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T

import Data.Binary.Tagged

data Field = Field
    { _fieldName  :: T.Text
    , _fieldValue :: Int
    }
  deriving (Eq, Show, Generic, Typeable)

instance Binary Field
instance NFData Field
instance Structured Field

data Record = Record
    { _recordFields  :: HM.HashMap T.Text Field
    , _recordEnabled :: Bool
    }
  deriving (Eq, Show, Generic, Typeable)

instance Binary Record
instance NFData Record
instance Structured Record

record :: Record
record = Record fields enabled
  where
    fields = HM.fromList (fmap mkField [1..1000])
    mkField i = let name = T.pack (show i)
                in (name, Field name i)
    enabled = True

encodedRecord :: LBS.ByteString
encodedRecord = encode record

taggedEncodedRecord :: LBS.ByteString
taggedEncodedRecord = structuredEncode record

main :: IO ()
main = defaultMain
  [ bgroup "encode"
      [ bench "Binary" $ nf encode record
      , bench "Tagged" $ nf structuredEncode record
      ]
  , bgroup "decode"
      [ bench "Binary" $ nf (decode :: LBS.ByteString -> Record) encodedRecord
      , bench "Tagged" $ nf (structuredDecode :: LBS.ByteString -> Record) taggedEncodedRecord
      ]
  ]
