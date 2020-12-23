module Main (main) where

import GetURL (getURL)
import Control.Monad (void, replicateM_)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, readMVar, forkIO)
import Text.Printf (printf)
import qualified Data.ByteString as B

newtype Async a = Async (MVar a)

sites :: [String]
sites =
  [ "http://www.google.com"
  , "http://www.bing.com"
  , "http://www.yahoo.com"
  , "http://www.wikipedia.com/wiki/Spade"
  , "http://www.wikipedia.com/wiki/Shovel"
  ]

main :: IO ()
main = do
  m <- newEmptyMVar
  let download url = getURL url >>= \r -> putMVar m (url, r)
  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar m
  printf "%s was first (%d bytes)\n" url (B.length r)
  -- wait for the rest of the results to arrive
  replicateM_ (length sites - 1) (takeMVar m)
