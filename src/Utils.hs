{-# LANGUAGE LambdaCase #-}
module Utils where

eitherFromMaybe :: e -> Maybe a -> Either e a
eitherFromMaybe e Nothing = Left e
eitherFromMaybe _ (Just a) = Right a


fromMaybeM :: Monad m => m b -> m (Maybe a) -> (a -> m b) -> m b
fromMaybeM onNothing mm action = mm >>= \case
  Nothing -> onNothing
  Just v ->  action v


withRightM :: Monad m => m (Either e a) -> (a->m (Either e b)) -> m (Either e b)
withRightM mm action = mm >>= \case
  Left e -> return $ Left e
  Right v ->  action v
