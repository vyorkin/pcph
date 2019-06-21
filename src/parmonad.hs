module Main (main) where

import System.Environment (getArgs)
import Control.Monad.Par (Par, IVar, runPar, new, get, put, fork)
import Control.DeepSeq (NFData)

-- Think:
-- fork - creates node
-- put - start edge
-- get - finish edge

-- (fib n) (fib m)
--    \     /
--    i\   /j
--      \ /
--    (a + b)

main :: IO ()
main = do
  args <- getArgs
  let [n, m] = map read args
  print $ runPar $ do
    -- create two new IVar's to hold the results
    i <- new
    j <- new
    -- fork independent Par computations
    fork $ put i (fib n)
    fork $ put j (fib m)
    -- wait for the results
    a <- get i
    b <- get j
    -- add the results and return
    return (a + b)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | Forks a computation in parallel and returns
-- an `IVar` that can be used to wait for the result.
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  i <- new
  fork $ do
    x <- p
    put i x
  return i

-- | Parallel map consists of calling `spawn` to apply the function
-- to each element of the list and then waiting for all the results.
parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap f as = do
  ibs <- mapM (spawn . return . f) as
  mapM get ibs
