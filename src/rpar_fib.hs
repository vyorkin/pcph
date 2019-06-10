module Main (main) where

import System.Environment (getArgs)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Monad (void)
import Control.Parallel.Strategies (Eval, runEvalIO, rpar, rseq)

main :: IO ()
main = do
  [n] <- getArgs
  let test = [test1, test2, test3, test4] !! (read n - 1)
  startTime <- getCurrentTime
  r <- runEvalIO test
  printTimeSince startTime
  print r
  printTimeSince startTime
  return ()

test1 :: Eval (Integer, Integer)
test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x, y)

test2 :: Eval (Integer, Integer)
test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x, y)

test3 :: Eval (Integer, Integer)
test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  void $ rseq x
  return (x, y)

test4 :: Eval (Integer, Integer)
test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  void $ rseq x
  void $ rseq y
  return (x, y)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

printTimeSince :: UTCTime -> IO ()
printTimeSince time = do
  now <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime now time) :: Double)
