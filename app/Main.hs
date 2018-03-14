{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.Environment
import Test

import Protolude

main :: IO ()
main =  do
  name <- flip atMay 0 <$> getArgs
  mainTest $ maybe (panic "Specify node name as first argument.") show name
