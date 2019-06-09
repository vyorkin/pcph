module Main (main) where

import Control.Parallel.Strategies (runEval, rpar, rseq)

main :: IO ()
main = print $ fn 2 3

-- | f x --------|-----
-- | f y -|------------
-- ------ | ret
-- ------------ time ->

-- Unlikely to be useful:
-- We rarely know in advance which of the two
-- computations takes the longest.

fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rseq (f y)
  return (a, b)
