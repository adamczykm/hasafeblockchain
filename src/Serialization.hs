{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Serialization where

import Protolude
import Data.Binary

data SerializationError = BinaryDecodingError Text

class Serializable a where
  serialize :: a -> LByteString

  default serialize :: Binary a => a -> LByteString
  serialize = encode

  deserialize :: LByteString -> Either SerializationError a
  default deserialize :: Binary a => LByteString -> Either SerializationError a
  deserialize = bimap (\(_,_, errString) -> BinaryDecodingError (show errString))
                      (\(_,_, a) -> a)
                . decodeOrFail
