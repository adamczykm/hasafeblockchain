{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Test.Tasty
import           Test.Tasty.HUnit

----
import Validation
import           Data.Either                      (isLeft)
import           StoredBlockchainManager.Internal
import           System.Directory
import           System.FilePath                  ((</>))
import           System.Random

  
import Control.Concurrent.STM
import Control.Concurrent.Async

main :: IO ()
main = defaultMain $ testGroup "Linked tests:"
  [ testTestSuite
  , tempStoredBlockchainTests ]


testTestSuite :: TestTree
testTestSuite = testGroup "Test test suite" $ return $ testCase "Test test case" $ assertBool "is True true?" True


tempStoredBlockchainTests :: TestTree
tempStoredBlockchainTests = withResource (prepareWorkingDirectory "stored_blockchain_tests_workdir")
                                         (const $ return ())
                                         -- removeDirectoryRecursive
                                          (\getWorkDir -> testGroup "Temp StoredBlockChainManager tests"
                                            [ tempStoredBlockchainReadTest getWorkDir
                                            , tempStoredBlockchainWriteBlock getWorkDir
                                            , appendReplaceAndReadTest getWorkDir
                                            , synchronizeEmptyWithStaticChain getWorkDir
                                            , synchronizeDynamicChain getWorkDir
                                            ])
  where
    prepareWorkingDirectory workDir = createDirectoryIfMissing True workDir >> return workDir
    testConfig getWorkDir testDir  = StoredBlockchainManagerConfig . (</> testDir) <$> getWorkDir

    assureDir cfg@(StoredBlockchainManagerConfig testDir) step = do
      void $ step "Assuring blockchain dir exists"
      _ <- runReaderT assureBlockchainDir cfg
      b <- doesPathExist testDir
      assertBool "Blockchain directory not created: " b

    clearStoredBlockchainTest cfg@(StoredBlockchainManagerConfig testDir) step = do
      void $ step "Clearing blockchain"
      ei <- try $ runReaderT clearStoredBlockchain cfg
      case ei of
        Right _ -> assertBool "Blockchain cleared." True
        Left (e :: IOError) -> assertFailure ("Error when cleaning blockchain in " ++ testDir ++ ": " ++ show e)

    assertBlockCount cfg count step = do
      void $ step ("Assert block count == " ++ show count)
      blockCount <- runReaderT getStoredBlockCount cfg
      assertEqual ("Block count should be equal to " ++ show count ) count blockCount

    failureOnLeft eith action = case eith of
      Left e  -> assertFailure $ show e
      Right a -> action a


    --- test case
    tempStoredBlockchainReadTest wrkDir = localOption (mkTimeout 1000000) $ testCaseSteps "Clearing and reading from empty directory" $ \step -> do
      cfg <- testConfig wrkDir "stored-blockchain-test1"

      assureDir cfg step
      clearStoredBlockchainTest cfg step

      ----------
      step "Reading from emptied blockchain dir."
      esb <- runReaderT readStoredBlockchain cfg
      assertEqual "There was a problem reading blockchain from existing empty directory" esb (Right ([] :: [Untrusted Int]))
      ----------

      assertBlockCount cfg 0 step

      step "Done"


    tempStoredBlockchainWriteBlock wrkDir = localOption (mkTimeout 1000000) $ testCaseSteps "Clearing and writing block" $ \step -> do
      cfg <- testConfig wrkDir "stored-blockchain-test2"

      assureDir cfg step
      clearStoredBlockchainTest cfg step
      assertBlockCount cfg 0 step

      ----------
      step "Appending block to in empty directory"
      ewb <- runReaderT (appendBlock (0 :: Int)) cfg
      failureOnLeft ewb return
      assertBlockCount cfg 1 step

      ----------
      step "Appending block in non-empty directory"
      ewb2 <- runReaderT (appendBlock (1 :: Int)) cfg
      case ewb2 of
        Left e   -> assertFailure $ show e
        Right () -> return ()
      assertBlockCount cfg 2 step

      ----------
      step "Replaceing last block"
      ewb3 <- runReaderT (replaceBlock 1 (1 :: Int)) cfg
      case ewb3 of
        Left e   -> assertFailure $ show e
        Right () -> return ()
      assertBlockCount cfg 2 step

      ----------
      step "Replaceing first block"
      ewb4 <- runReaderT (replaceBlock 0 (0 :: Int)) cfg
      case ewb4 of
        Left e   -> assertFailure $ show e
        Right () -> return ()
      assertBlockCount cfg 1 step

      step "Done"


    appendReplaceAndReadTest wrkDir = localOption (mkTimeout 1000000) $ testCaseSteps "Append blocks and read stored blockchain" $ \step -> do
      cfg <- testConfig wrkDir "stored-blockchain-test3"
      assureDir cfg step
      clearStoredBlockchainTest cfg step

      let blockCount = 10 :: Integer
      ----------
      step $ "Appending "++ show blockCount ++ " random blocks."
      blocks :: [Untrusted Int] <- replicateM (fromIntegral blockCount) (untrust <$> randomIO)
      e1 <- sequence <$> mapM (\b -> runReaderT (appendBlock b) cfg) blocks
      failureOnLeft e1 $ const $ do
        assertBlockCount cfg blockCount step

        ----------
        step "Reading the blocks and checking integrity."
        esb <- runReaderT readStoredBlockchain cfg

        failureOnLeft esb $ \(bs :: [Untrusted Int]) -> do
            assertEqual "Read blocks aren't equal to stored blocks" blocks bs

            step "Replaceing not existing block"
            nIx <- randomRIO (blockCount, 10*blockCount)
            er <- runReaderT (replaceBlock nIx (fromIntegral nIx :: Int)) cfg
            assertBool "Replaceing not existing block should return error" (isLeft er)

            step "Replaceing not-last block and checking integrity"
            eIx <- randomRIO (0, blockCount-2)

            let leftBlocks = take (fromIntegral eIx) blocks ++ [untrust $ fromIntegral eIx]
            er2 <- runReaderT (replaceBlock eIx (fromIntegral eIx :: Int)) cfg

            failureOnLeft er2 $ const $ do
              esb2 <- runReaderT readStoredBlockchain cfg
              failureOnLeft esb2 $ assertEqual "Read blocks aren't equal to stored blocks" leftBlocks



      ---------- synchronizeBlockchainsAndVerifyIntegrity step cfg blocks


    synchronizeEmptyWithStaticChain wrkDir = localOption (mkTimeout 1000000) $ testCaseSteps "Builds random static in-memory blockchain and synchronizes it." $ \step -> do
      cfg <- testConfig wrkDir "stored-blockchain-test4"
      assureDir cfg step
      clearStoredBlockchainTest cfg step

      let blockCount = 10 :: Int
      ----------
      step $ "Appending "++ show blockCount ++ " random blocks"
      blocks :: TVar [Int] <- newTVarIO =<< replicateM (fromIntegral blockCount) randomIO

      ----------
      synchronizeBlockchainsAndVerifyIntegrity step cfg blocks

      ----------
      step "Appending one block"
      randomIO >>= \nb -> atomically $ modifyTVar' blocks (\bs -> bs ++ [nb])
      synchronizeBlockchainsAndVerifyIntegrity step cfg blocks

      ----------
      step "Removing some blocks"
      atomically $ modifyTVar' blocks (take (blockCount `div` 2))
      synchronizeBlockchainsAndVerifyIntegrity step cfg blocks

      step "Done."


    synchronizeDynamicChain wrkDir = localOption (mkTimeout 1000000) $ testCaseSteps "Builds random static in-memory blockchain and synchronizes it." $ \step -> do
      cfg <- testConfig wrkDir "stored-blockchain-test5"
      assureDir cfg step
      clearStoredBlockchainTest cfg step

      let blockCount = 50 :: Int
      ----------
      step $ "Appending "++ show blockCount ++ " random blocks"
      blocks :: TVar [Int] <- newTVarIO =<< replicateM (fromIntegral blockCount) randomIO
      syncToken <- atomically $ newSynchronizationProcessToken (getMemBlockCount blocks) (getMemBlock blocks)

      step "Synchronizing"
      runReaderT (scheduleSynchronize syncToken) cfg
      ersync1 <- runReaderT (waitForLastSynchronizationResults syncToken) cfg
      failureOnLeft ersync1 $ const $ do
        step "Verifying integrity"
        er2 <- runReaderT readStoredBlockchain cfg
        memBlocksSnapshot <- (untrust <$>) <$> readTVarIO blocks
        failureOnLeft er2 $ assertEqual "Read blocks aren't equal to stored blocks" memBlocksSnapshot

      step "Modifying half of blocks while synchronizing"

      let replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls
          modifySomeBlocks = do
            forM_ [(blockCount `div` 2)..blockCount-1] $ \ix -> do
              b <- randomIO
              atomically $ modifyTVar' blocks (replaceAtIndex ix b)
            runReaderT (scheduleSynchronize syncToken) cfg

      (_,syncErr) <- concurrently modifySomeBlocks (runReaderT (waitForLastSynchronizationResults syncToken) cfg)
      failureOnLeft syncErr $ const $ return ()

      step "Waiting for synchronization results."
      syncErr2 <- runReaderT (waitForLastSynchronizationResults syncToken) cfg
      failureOnLeft syncErr2 $ const $ return ()
      step "Verifying integrity"
      er2 <- runReaderT readStoredBlockchain cfg
      memBlocksSnapshot <- (untrust <$>) <$> readTVarIO blocks
      failureOnLeft er2 $ assertEqual "Read blocks aren't equal to stored blocks" memBlocksSnapshot


    synchronizeBlockchainsAndVerifyIntegrity step cfg blocks = do
      void $ step "Synchronizinng blockchains"
      er1 <- runReaderT (synchronize (getMemBlockCount blocks) (getMemBlock blocks)) cfg
      failureOnLeft er1 $ const $ do

          void $ step "Verifying integrity"
          er2 <- runReaderT readStoredBlockchain cfg
          memBlocksSnapshot <- (untrust <$>) <$> readTVarIO blocks
          failureOnLeft er2 $ assertEqual "Read blocks aren't equal to stored blocks" memBlocksSnapshot

    getMemBlockCount blks = fromIntegral . length <$> readTVar blks

    getMemBlock blks ix = do
      bs <- readTVar blks
      let sz = length bs
      if fromIntegral ix >= sz || ix < 0 then return Nothing
        else return $ Just $ bs !! fromIntegral ix
