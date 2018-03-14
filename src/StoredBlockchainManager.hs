{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StoredBlockchainManager(synchronize, readStoredBlockchain) where

import           Protolude
import           Serialization
import           Validation
-- import MonadBlockchain
import           Control.Arrow        (left)
import qualified Data.ByteString.Lazy as BL
import           System.IO.Error

data StoredBlockchainManagerError = StoredBlockchainManagerSerializationError SerializationError
                                  | StoredBlockchainManagerFileReadError IOError


getBlockName ::  Integer -> [Char]
getBlockName  = (++ ".blk") . show

getStoredBlockCount :: MonadIO m => m Integer
getStoredBlockCount = return 10

readBlock :: (MonadIO m, Serializable a) => Integer -> m (Either StoredBlockchainManagerError a)
readBlock fname = do
  bindata <- liftIO $ try $ BL.readFile (getBlockName fname)
  return $ case bindata of
    Left e -> Left $ StoredBlockchainManagerFileReadError e
    Right bts -> left StoredBlockchainManagerSerializationError (deserialize bts)


readStoredBlockchain :: (MonadIO m, Serializable a) => m (Either StoredBlockchainManagerError [a])
readStoredBlockchain =  getStoredBlockCount >>= \cnt -> sequence <$> mapM readBlock [0..cnt-1]


writeBlock :: (MonadIO m, Serializable a) => Integer -> a -> m (Either StoredBlockchainManagerError ())
writeBlock fname block = do
  bindata <- liftIO $ try $ BL.writeFile (getBlockName fname) (serialize block)
  return $ left StoredBlockchainManagerFileReadError bindata


synchronize :: (MonadIO m, Eq a, Serializable a) => IO Integer -> (Integer -> IO a) -> m (Either StoredBlockchainManagerError ())
synchronize getMemBlockCount getMemBlock = liftIO (getStoredBlockCount >>= worker)
  where
    worker storedBlockCount = do
      memBlockCount <- getMemBlockCount
      -- if number of stored blocks is higher than in memory block then compare last possible
      if storedBlockCount > memBlockCount then worker memBlockCount
        else do
          eStoredBlock <- readBlock (storedBlockCount -1)
          case eStoredBlock of
            -- if expected stored block cannot be read, return error
            Left e -> return $ Left e
            -- else
            Right storedBlock -> do
              -- read last possible in-memmory block
              memBlock <- getMemBlock (memBlockCount -1)
              -- currently processed stored block and complementary in-memory block are equal
              if untrust memBlock == storedBlock then
                -- and this was the latest in-memory block, we can finish
                if storedBlockCount == memBlockCount then return (Right ())
                -- else try processing newer stored block again
                else worker (storedBlockCount + 1)
              -- currently processed stored block and complementary in-memory block are NOT equal
              else do
                -- overwrite stored block
                eOrOk <- writeBlock (storedBlockCount - 1) memBlock
                case eOrOk of
                  (Left err) -> return $ Left err
                  _          -> -- and recursively process previous stored block
                                worker (storedBlockCount - 1)
