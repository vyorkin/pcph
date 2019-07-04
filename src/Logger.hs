module Main (main) where

import Control.Monad (void)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)

newtype Logger = Logger (MVar LogCommand)

data LogCommand
  = Message String
  | Stop (MVar ())

main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l

-- if there are multiple threads trying to log messages at the
-- same time, it seems likely that the logging thread would not
-- be able to process the messages fast enough and most of the
-- threads would get blocked in logMessage while waiting for the
-- MVar to become empty

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  void $ forkIO $ logger l
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop where
  loop = do
    cmd <- takeMVar m
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop s -> do
        putStrLn "Logger: stop"
        putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
