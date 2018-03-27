{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module MemPool where

import           Data.Hashable
import qualified Data.HashPSQ  as PQ

import           Data.List     (foldl')

data MemPool a = forall p k. (Hashable k, Ord k, Ord p) => MemPool { getHash :: a -> k,
                                                                     getPriority :: a -> p,
                                                                     queue :: PQ.HashPSQ k p a}


emptyQueue :: (Hashable k, Ord k, Ord p) => (a->k) -> (a->p) -> MemPool a
emptyQueue hashFun priorityFun = MemPool hashFun priorityFun PQ.empty

addItem :: MemPool a -> a -> MemPool a
addItem MemPool{..} item = MemPool getHash getPriority (PQ.insert (getHash item) (getPriority item) item queue)

getTopItem :: MemPool a -> Maybe a
getTopItem MemPool{..} = (\(_,_,v) -> v) <$> PQ.findMin queue


getNTopItems :: MemPool a -> Int -> [a]
getNTopItems MemPool{..} n | n <= 0    = []
                           | otherwise = go [] n queue
  where
    go acc 0 _ = acc
    go acc i q = case PQ.findMin q  of
      Nothing      -> acc
      Just (k,_,v) -> go (v:acc) (i-1) (PQ.delete k q)



removeItem :: MemPool a -> a -> MemPool a
removeItem MemPool{..} = MemPool getHash getPriority . flip PQ.delete queue . getHash


removeItems :: MemPool a -> [a] -> MemPool a
removeItems MemPool{..} = MemPool getHash getPriority . foldl' (flip PQ.delete) queue . map getHash


size :: MemPool a -> Int
size MemPool{..} = PQ.size queue
