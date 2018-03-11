module Utils where

eitherFromMaybe :: e -> Maybe a -> Either e a
eitherFromMaybe e Nothing = Left e
eitherFromMaybe _ (Just a) = Right a
