module Main (main) where

import System.Environment (getArgs)
import Sudoku (solve)
import Data.Maybe (isJust)
import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Parallel.Strategies (Eval, runEval, rpar)

-- Dynamic partitioning

-- Compiling:
-- cabal new-build sudoku3 --ghc-options="-eventlog"

-- Running:
-- cabal new-exec sudoku3 sudoku17.1000.txt -- +RTS -s -l -RTS

-- sudoku1:  Total   time    0.030s  (  1.331s elapsed)
-- ----------------------- VS -------------------------
-- sudoku3:  Total   time    0.028s  (  0.311s elapsed)

-- wall-clock time difference:
-- ---------------------------
-- 1.331 / 0.311 ~= x 4.27

-- SPARKS: 1000(1000 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

-- We created exactly 1,000 sparks, and they were all converted
-- (that is, turned into real parallelism at runtime).

main :: IO ()
main = do
  [f]  <- getArgs
  file <- readFile f
  let
    puzzles = lines file
    solutions = runEval (parMap solve puzzles)

  -- Uncomment to see the eventlog with clear separation
  -- between the sequential and the parallel part.
  void $ evaluate (length puzzles)

  print (length (filter isJust solutions))

-- | Applies a function to a list in parallel.
--
-- Runs down the whole list, eagerly creating sparks for the application of the
-- given function to each element, and finally returns the new list.
--
-- When parMap returns, it will have created
-- one spark for each element of the list
parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
  b  <- rpar (f a)
  bs <- parMap f as
  return $ b:bs
