module Main where

import Lib

main :: IO ()
main = do 
  res <- httpCacheToIo $ runHttpCache "/tmp/cache" "https://www.haskell.org"
  print res
