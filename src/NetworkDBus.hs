{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module NetworkDBus where

import           Control.Arrow                 (left)
import           Control.Concurrent.Chan.Unagi (InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi as UC
import           Control.Exception
import           Data.Binary
import           Data.IORef
import           DBus.Client
import           DBus.Internal.Address         (Address)
import           DBus.Internal.Message
import           DBus.Internal.Types
import           NetworkProtocol
import           Protolude
import           System.IO.Error               (IOError)
import           Utils                         (eitherFromMaybe)


-- TODO make this right
-- versionString :: [Char]
versionString = "0.1.0"


-- networkApiVersionString :: [Char]
networkApiVersionString = "0/1/"


newtype DBusNetworkIO a = DBusNetworkIO { _unpackIO :: IO a }
  deriving(Functor, Applicative, Monad)


instance MonadIO DBusNetworkIO where
  liftIO = DBusNetworkIO

data DBusConnectionMode = DBusConnAddress Address
                        | DBusConnSystem
                        | DBusConnSession
                        | DBusConnStarter

data DBusNetworkState = MkDBusNetworkState {
  dbusClient  :: Maybe Client,
  msgQueueIn  :: InChan Signal,
  msgQueueOut :: OutChan Signal}

type DBusNetworkConfig = DBusConnectionMode
type DBusNetworkStateRef = IORef DBusNetworkState

instance MonadNetworkProtocol DBusNetworkIO where

  type NetworkConfig DBusNetworkIO = DBusNetworkConfig
  type NetworkState DBusNetworkIO = DBusNetworkStateRef

  getDefaultConfig = return DBusConnSession

  networkConnect cfg = left (clientToNetworkError NetworkConnectionError) <$> DBusNetworkIO (dbusConnect cfg)

  networkBroadcast nst = DBusNetworkIO . dbusBroadcast nst

  -- networkReceive :: Serializable a => NetworkState m -> ReceiveConfig -> m (Either NetworkError a)
  networkReceive nst = DBusNetworkIO . dbusReceive nst

  networkDisconnect = DBusNetworkIO . dbusDisconnect

  runNetwork = _unpackIO


dbusInterfaceName :: InterfaceName
dbusInterfaceName = fromMaybe (panic "Static error: DBusClientError: invalid interface name.")
  (parseInterfaceName "anks.hasafeblockchain.v.todo")


clientToNetworkError :: (Text -> NetworkError) -> ClientError -> NetworkError
clientToNetworkError constrNetError err = constrNetError $ show $  "DBusClientError: " <>  clientErrorMessage err


  -- networkReceive :: Serializable a => NetworkState m -> ReceiveConfig -> m (Either NetworkError a)
dbusReceive :: Binary a => DBusNetworkStateRef -> ReceiveConfig -> IO (Either NetworkError a)
dbusReceive dbusStateRef _ = do
  outChan <- msgQueueOut <$> readIORef dbusStateRef
  sigOrErr :: (Either IOError Signal) <- try $ UC.readChan outChan
  case sigOrErr of
    Left _    -> return $ Left NetworkTimeoutError
    Right sig -> return $ decode <$> decodeSignal sig

  where

    decodeSignal :: Signal -> Either NetworkError LByteString
    decodeSignal sig = do
      variant <- case messageBody sig of
        []    -> Left NetworkInvalidMessageFormat
        (v:_) -> Right v
      eitherFromMaybe NetworkInvalidMessageFormat (fromVariant variant)


dbusConnect :: DBusConnectionMode -> IO (Either ClientError DBusNetworkStateRef)
dbusConnect mode = do
  ceOrClient <- try $ mapModeConnect mode

  case ceOrClient of
    Left e -> return (Left e)
    Right cl -> do
      -- create network state
      (chIn, chOut) <- UC.newChan
      networkRef <- newIORef $ MkDBusNetworkState (Just cl) chIn chOut

      -- register for all signals matching interface name, and write them to network node queue
      void $ addMatch cl (matchAny { matchInterface = Just dbusInterfaceName}) (void . UC.writeChan chIn)

      return $ Right networkRef

   where
     mapModeConnect (DBusConnAddress address) = connect address
     mapModeConnect DBusConnSession           = connectSession
     mapModeConnect DBusConnSystem            = connectSystem
     mapModeConnect DBusConnStarter           = connectStarter


dbusDisconnect :: DBusNetworkStateRef -> IO DBusNetworkStateRef
dbusDisconnect dbusStateRef = do
  dbusState <- readIORef dbusStateRef
  case dbusClient dbusState of
    Just client -> disconnect client >> modifyIORef' dbusStateRef (\(MkDBusNetworkState _ chIn chOut) -> MkDBusNetworkState Nothing chIn chOut)
    Nothing     -> return ()
  return dbusStateRef


dbusBroadcast :: Binary a => DBusNetworkStateRef -> a -> IO (Either NetworkError ())
dbusBroadcast dbusStateRef msg = do
  dbusState <- readIORef dbusStateRef

  case buildSignal msg of
    Left e -> return $ Left e
    Right msgSig -> case dbusClient dbusState of
        Nothing -> return $ Left NetworkNotConnectedError
        Just client -> left (clientToNetworkError NetworkBroadcastError) <$> try (emit client msgSig)

  where

    buildSignal :: Binary a => a -> Either NetworkError Signal
    buildSignal msg' = do
      objPath <- eitherFromMaybe (NetworkBroadcastError "DbusClientError: invalid signal object path") $
        parseObjectPath ("/anks/hasafeblockchain/v/" <> networkApiVersionString <> "todo")

      membName <- eitherFromMaybe (NetworkBroadcastError "DbusClientError: invalid signal member name") $
        parseMemberName "testNameTodo"

      return $ Signal objPath dbusInterfaceName membName Nothing Nothing (buildMsgBody msg')

    buildMsgBody :: Binary a => a -> [Variant]
    buildMsgBody = return . toVariant . encode
