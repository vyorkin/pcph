module Main (main) where

import GetURL (getURL)
import Control.Monad (void)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, forkIO)
import Data.ByteString as B

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

main :: IO ()
main = do
  a1 <- async $ getURL "http://www.wikipedia.org/wiki/Shovel"
  a2 <- async $ getURL "http://www.wikipedia.org/wiki/Spade"
  -- we'll get a deadlock here if any of these 2 threads fail
  -- with exception before they `putMVar`
 -- (the runtime detects this and sends `BlockedIndefinitelyOnMVar`)
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)

-- The natural behavior would be for the error to be made
-- available to the thread that calls `wait` because that way the
-- caller can find out whether the asynchronous computation
-- returned an error or a result and act accordingly

-- Convenient behavior is for `wait` to simply propagate the
-- exception in the current thread so that in the common case
-- the programmer need not write any error-handling code at all.
