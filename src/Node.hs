{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Node where

import Protolude

import Types

data NodeError = BootNodeError Text

class Monad m => MonadNode m where

  data NodeConfig m :: *
  data Node m :: *

  bootNode :: NodeConfig m -> m (Either NodeError (Node m))

  



