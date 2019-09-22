{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Control.DeepSeq
import Data.ByteString.Lazy as LBS
import Data.Binary
import Data.Binary.Instances ()
import Data.Binary.Tagged
import Criterion.Main
import qualified Data.HashMap.Strict as HM
import Data.Text as T
import GHC.Generics

data Field = Field
  { _fieldName :: Text
  , _fieldValue :: Int
  }
  deriving (Eq, Show, Generic)

instance Binary Field
instance NFData Field
instance Structured Field

data Record = Record
  { _recordFields :: HM.HashMap Text Field
  , _recordEnabled :: Bool
  }
  deriving (Eq, Show, Generic)

instance Binary Record
instance NFData Record
instance Structured Record

record :: Record
record = Record fields enabled
  where fields = HM.fromList (fmap mkField [1..1000])
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
