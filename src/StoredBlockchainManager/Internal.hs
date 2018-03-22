{-# LANGUAGE RecordWildCards #-}
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           System.IO.Error
import    System.Directory
import    System.FilePath
import Data.Either (fromRight)
import Utils
import Control.Concurrent.STM
-- import Debug.Trace


-- debuging

data StoredBlockchainManagerError = StoredBlockchainManagerSerializationError SerializationError
                                  | StoredBlockchainManagerMemoryOperationError Text
                                  | StoredBlockchainManagerBlockDoesntExistError Integer
                                  | StoredBlockchainManagerFileReadError IOException
                                  | StoredBlockchainManagerFileWriteError IOException
                                  | StoredBlockchainManagerUnknownError IOException
                                  deriving(Show,Eq)

newtype StoredBlockchainManagerConfig = StoredBlockchainManagerConfig { blockchainDir :: FilePath}
  deriving(Read,Eq,Show)


getBlockPath :: MonadReader StoredBlockchainManagerConfig m => Integer -> m [Char]
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


readBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => Integer -> m (Either StoredBlockchainManagerError (Untrusted a))
readBlock ix = do
  fpath <- getBlockPath ix
  -- bindata <- liftIO $ try $ BL.readFile fpath

  bindata <- liftIO $ try $ BS.readFile fpath
  return $ case bindata of
    Left e -> Left $ StoredBlockchainManagerFileReadError e
    Right bts -> left StoredBlockchainManagerSerializationError (deserialize $ toS bts)



appendBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => a -> m (Either StoredBlockchainManagerError ())
appendBlock block = do
  fpath <- getStoredBlockCount >>= getBlockPath
  bindata <- liftIO $ try $ BL.writeFile fpath (serialize block)
  return $ left StoredBlockchainManagerFileWriteError bindata


unsafeReplaceBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => Integer -> a -> m (Either StoredBlockchainManagerError ())
unsafeReplaceBlock ix block = do
  fpath <- getBlockPath ix
  bindata <- liftIO $ try $ BL.writeFile fpath (serialize block)
  return $ left StoredBlockchainManagerFileWriteError bindata


replaceBlock :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => Integer -> a -> m (Either StoredBlockchainManagerError ())
replaceBlock ix block = do
  -- remove all blocks following the one we replaced.
  cnt <- getStoredBlockCount
  if ix >= cnt then return $ Left $ StoredBlockchainManagerBlockDoesntExistError ix
    else do
      mapM_ (getBlockPath >=> liftIO . removeFile) [ix..cnt-1]
      unsafeReplaceBlock ix block


clearStoredBlockchain :: (MonadReader StoredBlockchainManagerConfig m, MonadReader StoredBlockchainManagerConfig m, MonadIO m) => m ()
clearStoredBlockchain = getStoredBlockCount >>= \cnt -> mapM_ (getBlockPath >=> liftIO . removeFile) [0..cnt-1]


-- | Reads blockchain as it's stored. Returns lists of blocks.
-- WARNING: blocks should always be validated when building in-memory blockchain.
-- TODO: decide on a way to make it secure (return unsafety flag in types)
readStoredBlockchain :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Serializable a) => m (Either StoredBlockchainManagerError [Untrusted a])
readStoredBlockchain =  getStoredBlockCount >>= \cnt -> sequence <$> mapM readBlock [0..cnt-1]

--------------------------------------------- SYNCHRONIZATION -----------------------------------------------

-- REMARK: Synchronization assumes that blockchain is never visible in an inconsistent state.
--         Let's say blockchain is in the process of resolving fork. Currently blockchain ends
--         in blocks: ...,x1, x2, x3, x4,(*) but there's a longer chain available:
--                    ...,x1, x2, y1, y2, y3(**),
--         in that case we should switch to new chain atomically. Leaving visible to storing manager state:
--         ...,x1,x2,y1,x4, after (*) will not be properly synchronized as last block is the same.


data SynchronizationProcessToken a = SynchronizationProcessToken{
  getMemBlockCount :: STM Integer,
  getMemBlock :: Integer -> STM (Maybe a),
  nextIsScheduled :: TVar (Maybe ()),
  currentSynchronizationAsync :: TVar (Either (Async (Either StoredBlockchainManagerError ())) (Either StoredBlockchainManagerError ()))}

newSynchronizationProcessToken :: STM Integer -> (Integer -> STM (Maybe a)) -> STM (SynchronizationProcessToken a)
newSynchronizationProcessToken getMemBlockCountAction getMemBlockAction = do
  nextTvr <- newTVar Nothing
  syncTvr <- newTVar (Right (Right ()))
  return $ SynchronizationProcessToken getMemBlockCountAction getMemBlockAction nextTvr syncTvr


waitForLastSynchronizationResults :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Eq a, Serializable a) =>
                                      SynchronizationProcessToken a -> m (Either StoredBlockchainManagerError ())
waitForLastSynchronizationResults tk@(SynchronizationProcessToken _ _ nextScheduledTVar syncStatusTVar) = do

  (nextScheduled, syncStatus) <- liftIO $ atomically $ do
    ns <- readTVar nextScheduledTVar
    ss <- readTVar syncStatusTVar
    return (ns,ss)

  case syncStatus of
    -- currently theres no synchronization in process
    Right x   -> case nextScheduled of
        -- and no synchronization is scheduled
        Nothing -> return x
        -- next synchronization is scheduled
        _       -> synchronizeAndWait
    -- there's synchronization in process
    Left task -> do
      -- wait till it's finished, merge errors
      ee <- fmap (join . left StoredBlockchainManagerUnknownError) <$> liftIO $ try $ wait task

      -- mark synchronization as finished
      liftIO $ atomically $ writeTVar syncStatusTVar (Right ee)

      case nextScheduled of
        -- next synchronization is scheduled
        Just _ -> synchronizeAndWait
        -- no synchronization is scheduled
        Nothing -> return ee

  where
    synchronizeAndWait = do
        liftIO $ atomically (writeTVar nextScheduledTVar Nothing)
        -- schedule synchronization
        scheduleSynchronize tk
        -- and start waiting again
        waitForLastSynchronizationResults tk



scheduleSynchronize :: (MonadReader StoredBlockchainManagerConfig m,
                        MonadIO m, Eq a, Serializable a) => SynchronizationProcessToken a -> m ()
scheduleSynchronize SynchronizationProcessToken{..} = do

  (nextScheduled, syncStatus) <- liftIO $ atomically $ do
    ns <- readTVar nextIsScheduled
    ss <- readTVar currentSynchronizationAsync
    return (ns,ss)

  case syncStatus of
    -- currently theres no synchronization in process
    Right _   -> do
      -- if theres synchronization scheduled, unschedule
      when (isJust nextScheduled) (liftIO $ atomically (writeTVar nextIsScheduled Nothing))
      -- and synchronize
      cfg <- ask
      task <- liftIO $ async (runReaderT (synchronize getMemBlockCount getMemBlock) cfg)
      liftIO $ atomically $ writeTVar currentSynchronizationAsync (Left task)

    -- there's synchronization in process
    Left _ -> -- set next synchro scheduled flag
      liftIO $ atomically (writeTVar nextIsScheduled (Just ()))


-- | Given a way to access current blockchain data synchronizes its stored counterpart.
-- This function will return only after entire chains are equal.
-- If blockchain gets updated rapidly and storage is really slow it might never finish.
-- But that's very unlikel
synchronize :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m, Eq a, Serializable a) => STM Integer -> (Integer -> STM (Maybe a)) -> m (Either StoredBlockchainManagerError ())
synchronize getMemBlockCount getMemBlock = do
  memCnt <- getMemBlockCountM
  cnt <- getStoredBlockCount
  -- remove blocks with higher ix then highest in-memory
  mapM_ (getBlockPath >=> liftIO . removeFile) [memCnt..cnt-1]
  -- start synchronization of the rest
  worker (memCnt -1)
  where
    worker :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m) => Integer -> m (Either StoredBlockchainManagerError ())
    worker currentBlockIx =
      if currentBlockIx < 0 then return $ Right () -- all blocks are processed
        else do
          memBlockCount <- getMemBlockCountM
          -- if number of stored blocks is higher than in memory block then compare last possible
          if currentBlockIx >= memBlockCount then worker (memBlockCount-1)
            else
              -- retrieve in memory block and return if any errors
              fromMaybeM (return $ Left $ StoredBlockchainManagerMemoryOperationError "TODO")
                (liftIO $ atomically $ getMemBlock currentBlockIx) $ \(memBlock :: a) -> do

                eStoredBlock <- readBlock currentBlockIx

                -- currently processed stored block exists and is equal to complementary in-memory block
                if isRight eStoredBlock && fromRight (panic "Either should be guaranteed to be Right") eStoredBlock == untrust memBlock then
                  -- and this was the latest in-memory block, we can finish
                  if currentBlockIx + 1 == memBlockCount then return (Right ())
                  -- else try processing newer stored block again
                  else worker (currentBlockIx + 1)

                else -- overwrite stored bloc
                  withRightM (unsafeReplaceBlock currentBlockIx memBlock) $ \_ ->
                    -- and recursively process previous stored block
                    worker (currentBlockIx - 1)

    getMemBlockCountM :: (MonadReader StoredBlockchainManagerConfig m, MonadIO m) =>  m Integer
    getMemBlockCountM = liftIO $ atomically getMemBlockCount
