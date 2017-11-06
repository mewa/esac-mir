module Main where

import Server
import System.Environment

main :: IO ()
main = getArgs >>= serve . read . head
