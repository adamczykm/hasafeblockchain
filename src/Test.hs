{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import           Control.Concurrent  (threadDelay)
import           Control.Monad.Trans (liftIO)

import NetworkDBus
import NetworkProtocol

import Data.Time.Clock
import Data.Time.Format

import Data.Text (concat)

import Protolude


mainTest = _unpackIO . testM


testM :: MonadNetworkProtocol m => [Char] -> m ()
testM nm = do
  eSt <- getDefaultConfig >>= networkConnect
  case eSt of
    Left e -> liftIO $ putText $ show e
    Right st -> forever $ do
      t <- show . formatTime defaultTimeLocale "%s%Q" <$> liftIO getCurrentTime
      merr <- networkBroadcast st (show nm :: Text, 1 :: Int, t :: Text)
      liftIO $ case merr of
        Left e -> putText $ show e
        Right () -> return ()

      merr <- networkBroadcast st (1 :: Int)
      liftIO $ case merr of
        Left e -> putText $ show e
        Right () -> return ()


      Right (num :: Int) <- networkReceive st (ReceiveBlock Nothing)
      putText $ Data.Text.concat ["Received num: ", show num, " at"]


      Right senderName <- networkReceive st (ReceiveBlock Nothing)

      t2 <- show . formatTime defaultTimeLocale "%s%Q" <$> liftIO getCurrentTime
      putText $ Data.Text.concat ["Received: from", senderName, " at", t2]

      liftIO $ threadDelay 1000000

  return ()
