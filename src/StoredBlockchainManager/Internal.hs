{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StoredBlockchainManager.Internal where

import           Protolude
import           Serialization
import           Validation
-- import MonadBlockchain
import           Control.Arrow        (left)
import qualified Data.ByteString.Lazy as BL
import           System.IO.Error
import    System.Directory
import    System.FilePath


-- debuging

data StoredBlockchainManagerError = StoredBlockchainManagerSerializationError SerializationError
                                  | StoredBlockchainManagerBlockDoesntExistError Integer
                                  | StoredBlockchainManagerFileReadError IOException
                                  | StoredBlockchainManagerFileWriteError IOException
                                  deriving(Show,Eq)

data StoredBlockchainManagerConfig = StoredBlockchainManagerConfig { blockchainDir :: FilePath}
  deriving(Read,Eq,Show)


getBlockPath ::  (MonadReader StoredBlockchainManagerConfig m) => Integer -> m [Char]
getBlockPath ix = do
  dir <- blockchainDir <$> ask
  return $ dir </> (show ix ++ ".blk")


assureBlockchainDir :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m) => m Bool
assureBlockchainDir = do
  dir <- blockchainDir <$> ask
  r <- liftIO $ doesPathExist dir
  liftIO $ createDirectoryIfMissing True dir
  return r


getStoredBlockCount :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m) => m Integer
getStoredBlockCount = assureBlockchainDir >>= bool (return 0) (hasNext 0)
  where
    hasNext count = do
      fname <- getBlockPath count
      eexist :: Either IOError Bool <- liftIO $ try $ doesFileExist fname
      case eexist of
        Right True -> hasNext (count+1)
        _ -> return count


readBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => Integer -> m (Either StoredBlockchainManagerError a)
readBlock ix = do
  fpath <- getBlockPath ix
  bindata <- liftIO $ try $ BL.readFile fpath
  return $ case bindata of
    Left e -> Left $ StoredBlockchainManagerFileReadError e
    Right bts -> left StoredBlockchainManagerSerializationError (deserialize bts)



appendBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => a -> m (Either StoredBlockchainManagerError ())
appendBlock block = do
  fpath <- getStoredBlockCount >>= getBlockPath
  bindata <- liftIO $ try $ BL.writeFile fpath (serialize block)
  return $ left StoredBlockchainManagerFileReadError bindata

replaceBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => Integer -> a -> m (Either StoredBlockchainManagerError ())
replaceBlock ix block = do
  -- remove all blocks following the one we replaced.
  cnt <- getStoredBlockCount
  if ix >= cnt then return $ Left $ StoredBlockchainManagerBlockDoesntExistError ix
    else do
      mapM_ (getBlockPath >=> liftIO . removeFile) [ix..cnt-1]
      fpath <- getBlockPath ix
      bindata <- liftIO $ try $ BL.writeFile fpath (serialize block)
      return $ left StoredBlockchainManagerFileWriteError bindata


clearStoredBlockchain :: (MonadReader StoredBlockchainManagerConfig m, MonadReader StoredBlockchainManagerConfig m, MonadIO m) => m ()
clearStoredBlockchain = getStoredBlockCount >>= \cnt -> mapM_ (getBlockPath >=> liftIO . removeFile) [0..cnt-1]


-- | Reads blockchain as it's stored. Returns lists of blocks.
-- WARNING: blocks should always be validated when building in-memory blockchain.
-- TODO: decide on a way to make it secure (return unsafety flag in types)
readStoredBlockchain :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => m (Either StoredBlockchainManagerError [a])
readStoredBlockchain =  getStoredBlockCount >>= \cnt -> sequence <$> mapM readBlock [0..cnt-1]


-- | Given a way to access current blockchain data synchronizes its stored counterpart.
-- This function will return only after entire chains are equal.
-- If blockchain gets updated rapidly and storage is really slow it might never finished.
-- But that's very unlikely.
synchronize :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Eq a, Serializable a) => IO Integer -> (Integer -> IO a) -> m (Either StoredBlockchainManagerError ())
synchronize getMemBlockCount getMemBlock = getStoredBlockCount >>= worker
  where
    worker :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m) => Integer -> m (Either StoredBlockchainManagerError ())
    worker storedBlockCount = do
      memBlockCount <- liftIO getMemBlockCount
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
              memBlock <- liftIO $ getMemBlock (memBlockCount -1)
              -- currently processed stored block and complementary in-memory block are equal
              if untrust memBlock == storedBlock then
                -- and this was the latest in-memory block, we can finish
                if storedBlockCount == memBlockCount then return (Right ())
                -- else try processing newer stored block again
                else worker (storedBlockCount + 1)
              -- currently processed stored block and complementary in-memory block are NOT equal
              else do
                -- overwrite stored block
                eOrOk <- appendBlock memBlock
                case eOrOk of
                  (Left err) -> return $ Left err
                  _          -> -- and recursively process previous stored block
                                worker (storedBlockCount - 1)

