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

-- -N2                                  total    elapsed
-- Gen  0       126 colls,   126 par    0.066s   0.050s     0.0004s    0.0012s
-- Gen  1         7 colls,     6 par    0.073s   0.038s     0.0054s    0.0120s

-- -N1                                  total    elapsed
-- Gen  0       126 colls,     0 par    0.026s   0.026s     0.0002s    0.0005s
-- Gen  1         7 colls,     0 par    0.044s   0.044s     0.0063s    0.0146s

-- 0.050 / 0.026 ~= 1.92

-- It runs ~2 times slower on 2 cores than on a single core
