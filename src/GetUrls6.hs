module Main (main) where

import GetURL (getURL)
import Control.Monad (void, replicateM_)
import Control.Exception (SomeException, try, throwIO)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, readMVar, forkIO)
import Text.Printf (printf)
import qualified Data.ByteString as B

newtype Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- try action
    putMVar var r
  return $ Async var

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e  -> throwIO e
    Right v -> return v

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = do
  m <- newEmptyMVar
  void $ forkIO $ do
    r <- try (Left <$> wait a)
    putMVar m r
  void $ forkIO $ do
    r <- try (Right <$> wait b)
    putMVar m r
  wait $ Async m

waitAny :: [Async a] -> IO a
waitAny as = do
  m <- newEmptyMVar
  let
    forkwait a = forkIO $ do
      r <- try $ wait a
      putMVar m r
  mapM_ forkwait as
  wait $ Async m

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
  let download url = getURL url >>= \r -> return (url, r)
  as <- mapM (async . download) sites
  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  -- wait for the rest of the results to arrive
  replicateM_ (length sites - 1) (takeMVar m)
