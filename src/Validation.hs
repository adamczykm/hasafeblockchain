{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Validation(Untrusted, untrust, ValidationError(..)) where

import Protolude
import Serialization
import Data.Binary


data ValidationError = InvalidContract Text
  deriving(Eq,Show)

newtype Untrusted a = Untrusted a
  deriving(Show, Eq, Binary, Serializable)

-- instance (Serializable a) => Serializable (Untrusted a) where
--   serialize = serialize
--   deserialize = right untrust . deserialize

untrust :: a -> Untrusted a
untrust = Untrusted
