{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import           Control.Concurrent  (threadDelay)
import           Control.Monad.Trans (liftIO)
import           P2P

import NetworkDBus
import NetworkProtocol


import Control.Distributed.Process
import Control.Distributed.Process.Node

import Data.Time.Clock
import Data.Time.Format

import Data.Text (concat)

import Protolude


mainTest = test2


test2 :: [Char]-> IO ()
test2 nm = _unpackIO $ do
  eSt <- DBusNetworkIO (dbusConnect DBusConnSession)
  case eSt of
    Left e -> liftIO $ putText $ show e
    Right st -> forever $ do
      t <- show . formatTime defaultTimeLocale "%s%Q" <$> liftIO getCurrentTime
      merr <- DBusNetworkIO $ dbusBroadcast st ((show nm) :: Text, t :: Text)
      liftIO $ case merr of
        Left e -> putText $ show e
        Right () -> return ()
      Right (senderName :: Text, sentTime :: Text) <- DBusNetworkIO $ dbusReceive  st (ReceiveBlock Nothing)

      t2 <- show . formatTime defaultTimeLocale "%s%Q" <$> liftIO getCurrentTime
      putText $ Data.Text.concat ["Received: ", sentTime, " from", senderName, " at", t2]
      liftIO $ threadDelay 1000000

  return ()

-- test3 :: IsString s => s -> IO ()
-- test3 nm = runNetwork $ do
--   eSt <- getDefaultConfig >>= networkConnect
--   case eSt of
--     Left e -> liftIO $ putText $ show e
--     Right st -> forever $ do
--       t <- liftIO getCurrentTime
--       merr <- networkBroadcast st ((show nm) :: Text,t)
--       liftIO $ case merr of
--         Left e -> putText $ show e
--         Right () -> return ()
--       Right (senderName :: Text, sentTime :: UTCTime) <- networkReceive st (ReceiveBlock Nothing)
--       threadDelay 1000

--   return ()


-- test1=undefined
-- serviceName = "9000"
-- hostname = "127.0.0.1"
-- hostnameExt = "legion"
-- port = show 9001


-- test1 port = P2P.bootstrap hostname port (\srvc -> (hostname, srvc)) initRemoteTable [P2P.makeNodeId "127.0.0.1:9000"] $ do
--     liftIO $ threadDelay 10000000 -- give dispatcher a second to discover other nodes
--     P2P.nsendPeers port "some"
--     p :: String <- expect
--     liftIO $ print p

