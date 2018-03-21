{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Serialization where

import           Data.Binary
import           Protolude

data SerializationError = BinaryDecodingError Text
  deriving(Read,Show,Eq)

class Serializable a where
  serialize :: a -> LByteString

  default serialize :: Binary a => a -> LByteString
  serialize = encode

  deserialize :: LByteString -> Either SerializationError a
  default deserialize :: Binary a => LByteString -> Either SerializationError a
  deserialize = bimap (\(_,_, errString) -> BinaryDecodingError (show errString))
                      (\(_,_, a) -> a)
                . decodeOrFail


instance Serializable Int
instance Serializable Integer

