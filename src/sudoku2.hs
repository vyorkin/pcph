module Main (main) where

import System.Environment (getArgs)
import Sudoku (solve)
import Data.Maybe (isJust)
import Control.Monad (void)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (runEval, rpar, rseq)

-- Total   time    0.054s  (  1.260s elapsed)
-- ------------------ VS --------------------
-- Total   time    0.041s  (  0.840s elapsed)

-- ~ x 1.5

main :: IO ()
main = do
  [f]  <- getArgs
  file <- readFile f
  let
    puzzles = lines file
    (as, bs) = splitAt (length puzzles `div` 2) puzzles
    solutions = runEval $ do
      as' <- rpar (force (map solve as))
      bs' <- rpar (force (map solve bs))
      void $ rseq as'
      void $ rseq bs'
      return (as' ++ bs')

  print (length (filter isJust solutions))
