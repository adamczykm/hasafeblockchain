{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Test.Tasty
import           Test.Tasty.HUnit

----
import           Data.Either                      (isLeft)
import           StoredBlockchainManager.Internal
import           System.Directory
import           System.FilePath                  ((</>))
import           System.Random

main :: IO ()
main = defaultMain $ testGroup "Linked tests:"
  [ testTestSuite
  , tempStoredBlockchainTests ]


testTestSuite :: TestTree
testTestSuite = testGroup "Test test suite" $ return $ testCase "Test test case" $ assertBool "is True true?" True


tempStoredBlockchainTests :: TestTree
tempStoredBlockchainTests = withResource (prepareWorkingDirectory "stored_blockchain_tests_workdir")
                                         removeDirectoryRecursive
                                          (\getWorkDir -> testGroup "Temp StoredBlockChainManager tests"
                                            [ tempStoredBlockchainReadTest getWorkDir
                                            , tempStoredBlockchainWriteBlock getWorkDir
                                            , appendReplaceAndReadTest getWorkDir])
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
      assertEqual "There was a problem reading blockchain from existing empty directory" esb (Right ([] :: [Int]))
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
      blocks :: [Int] <- replicateM (fromIntegral blockCount) randomIO
      e1 <- sequence <$> mapM (\b -> runReaderT (appendBlock b) cfg) blocks
      failureOnLeft e1 $ const $ do
        assertBlockCount cfg blockCount step

        ----------
        step "Reading the blocks and checking integrity."
        esb <- runReaderT readStoredBlockchain cfg

        failureOnLeft esb $ \(bs :: [Int]) -> do
            assertEqual "Read blocks aren't equal to stored blocks" blocks bs

            step "Replaceing not existing block"
            nIx <- randomRIO (blockCount, 10*blockCount)
            er <- runReaderT (replaceBlock nIx (fromIntegral nIx :: Int)) cfg
            assertBool "Replaceing not existing block should return error" (isLeft er)

            step "Replaceing not-last block and checking integrity"
            eIx <- randomRIO (0, blockCount-2)

            let leftBlocks = take (fromIntegral eIx) blocks ++ [fromIntegral eIx]
            er2 <- runReaderT (replaceBlock eIx (fromIntegral eIx :: Int)) cfg

            failureOnLeft er2 $ const $ do
              esb2 <- runReaderT readStoredBlockchain cfg
              failureOnLeft esb2 $ assertEqual "Read blocks aren't equal to stored blocks" leftBlocks

