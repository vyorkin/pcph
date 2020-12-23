module Main (main) where

import GetURL (getURL)
import Control.Monad (void)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)
import Data.ByteString as B

main :: IO ()
main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  void $ forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  void $ forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  r2 <- takeMVar m2
  print (B.length r1, B.length r2)
