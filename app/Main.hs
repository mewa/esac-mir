module Main where

import Server
import System.Environment

main :: IO ()
main = getEnv "PORT" >>= serve . read
