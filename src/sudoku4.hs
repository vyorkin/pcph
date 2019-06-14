module Main (main) where

import System.Environment (getArgs)
import Sudoku (solve)
import Data.Maybe (isJust)
import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Parallel.Strategies (Eval, runEval, rpar, rseq, using, parList)

-- `using` parList

-- Compiling:
-- cabal new-build sudoku4 --ghc-options="-eventlog"

-- Running:
-- cabal new-exec sudoku4 sudoku17.1000.txt -- +RTS -s -l -RTS

main :: IO ()
main = do
  [f]  <- getArgs
  file <- readFile f
  let
    puzzles = lines file
    solutions = solve <$> puzzles `using` parList rseq
    -- solutions = runEval (parMap solve puzzles)

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
