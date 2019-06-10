module Main (main) where

import Control.Monad (void)
import Control.Parallel.Strategies (runEval, rpar, rseq)

main :: IO ()
main = print $ fn 2 3

-- | f x ------a------------
-- | f y -b-----------------
-- ------------| ret
-- ----------------- time ->

-- The code waits until both `f x` and `f y` have
-- completed evaluation before returning.

-- Use when you need the results of
-- one of the operations in order to continue.

fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rseq (f y)
  void $ rseq a
  return (a, b)
