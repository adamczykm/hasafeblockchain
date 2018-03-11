{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module NetworkProtocol where

import Protolude

import Data.Binary (Binary)
import Types


class (Binary a, Typeable a) => Serializable a


data NetworkError = NetworkConnectionError Text
                  | NetworkBroadcastError Text
                  | NetworkTimeoutError
                  | NetworkNotConnectedError
                  | NetworkInvalidMessageFormat
                  deriving(Show,Eq)


data ReceiveConfig = ReceiveBlock (Maybe Integer)
                   | ReceiveNonBlock (Maybe Integer)
                   deriving(Show, Eq)

class MonadIO m => MonadNetworkProtocol m where

  type NetworkConfig m :: *
  type NetworkState m :: *

  runNetwork :: m a -> IO a
 
  getDefaultConfig :: m (NetworkConfig m)

  networkConnect :: NetworkConfig m -> m (Either NetworkError (NetworkState m))

  networkBroadcast :: Serializable msg => NetworkState m -> msg -> m (Either NetworkError ())

  networkReceive :: Serializable a => NetworkState m -> ReceiveConfig -> m (Either NetworkError a)

  networkDisconnect :: NetworkState m -> m (NetworkState m)










