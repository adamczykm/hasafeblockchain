{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}



module Dev() where
-- This module contains protyped code and does not export anything.
-- Following are some initial ideas that will be factored out
-- and put into different modules

import AsyncChain
import Control.Monad.IO.Class



------------- a blockchain monad

-- a high-level monadic interface through which external world can manipulate blockchain


data BlockTag = ABlockTag -- TODO what block tags do we need from this interface?
data ContractTag

data ContractValidityStatus
data ContractConfirmationStatus a

class MonadBlockchainNode m where

  type Block m :: BlockTag -> *

  type Contract m :: ContractTag -> *

  type Blockchain m :: *

  tryIncludeContract :: Contract m a -> m (AsyncChain '[ContractValidityStatus, ContractConfirmationStatus (Block m ABlockTag)])



-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeFamilies #-}

-- module MonadBlockchain where

-- import Serialization
-- import Validation

-- class (Monad m, Serializable (Untrusted (Contract m))) => MonadBlockchain m where

--   type (Block m) :: *
--   type (Contract m) :: *


------ Async chain - an error or an Async returning an Async chain

---------- in async chain module




----- Control Flow

--

startNode :: IO ()
startNode = do

  -- read configs
  -- concurrently:
    -- read stored blockchain + initialize validator + validate it
    -- initialize communication layer + boot network connection

  -- boot mempool and memblockchain manager
  -- boot user communication module

  return ()




----------------------------------------------------------
--------------  mining -----------------------


data MiningError


-- how to factor out callbacks
-- TODO read on callbacks in haskell, what are alternatives? pipes?
type BlockMinedCallback bl = Either MiningError bl -> IO ()

data MiningArgs bl  -- temp. also new not yet "mined block"

data MiningProcess  -- handle for runnning process






-- monadio for concurrecy - consider factoring out
-- start
startMiningProcess :: MonadIO m => MiningArgs bl -> BlockMinedCallback bl -> m MiningProcess
startMiningProcess = undefined

-- interrupt currently ongoing mining process and pass new mining arguments
-- use this whenever blockchain has changed and currently mined process would be
-- invalid in new context
interruptMiningProcess :: MonadIO m => MiningProcess -> MiningArgs bl -> m ()
interruptMiningProcess = undefined
