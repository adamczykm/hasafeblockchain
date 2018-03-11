module Types where

import Data.Set

data Ledger = Ledger

data Block = MkBlock { header :: BlockHeader,
                       txx    :: Set Tx
                     }

data BlockHeader

newtype BlockChain = MkBlockChain [Block]

data Tx = MkTx

newtype Address = MkAddress ()
newtype PubKey = MkPubKey ()
newtype PrivKey = MkPrivKey ()
