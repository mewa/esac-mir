module Main where

import Server
import System.Environment

main :: IO ()
main = do
  port <- fmap read $ getEnv "PORT"
  dbconnstr <- getEnv "DB_CONN"
  serve port dbconnstr
