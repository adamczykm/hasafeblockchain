{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Safe where

import           Control.Arrow (left)

newtype Safe f e a = MkSafe { getSafe :: Either (f e) a}
  deriving(Functor,Applicative,Monad)

safe :: (e -> f d) -> Either e a -> Safe f d a
safe lft = MkSafe . left lft

fromSafe :: Applicative f => a -> Safe f e a -> f a
fromSafe defVal = fromSafeHandle (const defVal)

fromSafeHandle :: Applicative f => (e -> a) -> Safe f e a -> f a
fromSafeHandle hndl (MkSafe eith) = case eith of
  Left err -> hndl <$> err
  Right vl -> pure vl
