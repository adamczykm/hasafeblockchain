{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Validation(Untrusted, untrust, ValidationError(..)) where

import Protolude
import Serialization
import Data.Binary


data ValidationError = MarkedAsInValidError
  deriving(Eq,Show)

newtype Untrusted a = Untrusted a
  deriving(Show, Eq, Binary, Serializable)

-- instance (Serializable a) => Serializable (Untrusted a) where
--   serialize = serialize
--   deserialize = right untrust . deserialize

untrust :: a -> Untrusted a
untrust = Untrusted


------------- provisional validator implementation -----------------

newtype Validator e m a = MkValidator { validFun :: Untrusted a -> m (Either e a) }


makeValidator :: Functor m => (Untrusted a -> m Bool) -> Validator ValidationError m a
makeValidator boolFun = MkValidator validationFun
  where validationFun x@(Untrusted trusted) = flip fmap (boolFun x) $ \case
          False -> Left MarkedAsInValidError
          True  -> Right trusted

makeValidatorWithCustomError :: Functor m => (Untrusted a -> m (Either e a)) -> Validator e m a
makeValidatorWithCustomError = MkValidator

validate :: Validator e m a -> Untrusted a -> m (Either e a)
validate (MkValidator vfun) = vfun
