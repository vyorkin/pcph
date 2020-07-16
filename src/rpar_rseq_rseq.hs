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

-- -N2                                  total    elapsed
-- Gen  0       126 colls,   126 par    0.050s   0.039s     0.0003s    0.0007s
-- Gen  1         7 colls,     6 par    0.064s   0.033s     0.0048s    0.0096s

-- -N1                                  total    elapsed
-- Gen  0       126 colls,     0 par    0.028s   0.028s     0.0002s    0.0005s
-- Gen  1         7 colls,     0 par    0.043s   0.043s     0.0061s    0.0138s

-- 0.039 / 0.028 ~= 1.39

-- It runs ~1.4 times slower on 2 core than on a single core
