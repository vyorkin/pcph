module Main (main) where

import GetURL (getURL)
import Control.Monad (void)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, forkIO)
import Data.ByteString as B
import TimeIt (timeit)
import Text.Printf (printf)

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- action
    putMVar var r
  return $ Async var

wait :: Async a -> IO a
wait (Async var) = readMVar var

sites :: [String]
sites =
  [ "http://www.google.com"
  , "http://www.bing.com"
  , "http://www.yahoo.com"
  , "http://www.wikipedia.com/wiki/Spade"
  , "http://www.wikipedia.com/wiki/Shovel"
  ]

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main :: IO ()
main = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as
