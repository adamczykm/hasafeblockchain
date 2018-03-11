module Main where

import System.Environment
import Test

main :: IO ()
main =  do
  port <- (!! 0) <$> getArgs
  mainTest port
