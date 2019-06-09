module Main (main) where

import Control.Parallel.Strategies (runEval, rpar)

main :: IO ()
main = print $ fn 2 3

-- | f x --------------
-- | f y --------------
-- | ret
-- ------------ time ->

-- Use when you don't depend on the results of either computation.

fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rpar (f y)
  return (a, b)
