module Main (main) where

import GHC.Conc (numCapabilities)
import System.Environment (getArgs)
import Sudoku (solve)
import Data.Maybe (isJust)
import Control.Monad (void)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (runEval, rpar, rseq)

-- Static partitioning

-- Compiling:
-- cabal new-build sudoku2 --ghc-options="-eventlog"

-- Running:
-- cabal new-exec sudoku2 sudoku17.1000.txt -- +RTS -s -l -RTS

-- The argument +RTS -s instructs the GHC runtime system to emit the statistics
-- shown.

-- As of GHC v6.12, you can leave off the number of cores and all available
-- cores will be used (you still need to pass -N however, like so: +RTS -N).

-- Total   time    0.054s  (1.260s elapsed)
-- ------------------ VS ------------------
-- Total   time    0.041s  (0.840s elapsed)

-- wall-clock time difference:
-- ---------------------------
-- 1.260 / 0.041 ~= x 1.5

-- Try to avoid partitioning the work into a small,
-- fixed number of chunks

main :: IO ()
main = do
  putStrLn $ "N (cores) = " ++ show numCapabilities
  [f]  <- getArgs
  file <- readFile f
  let
    puzzles = lines file
    (as, bs) = splitAt (length puzzles `div` 2) puzzles
    solutions = runEval $ do
      -- Not evaluating deeply enough is a
      -- common mistake when using `rpar`
      as' <- rpar $ force (solve <$> as)
      bs' <- rpar $ force (solve <$> bs)
      void $ rseq as'
      void $ rseq bs'
      return (as' ++ bs')

  print (length (filter isJust solutions))

-- What we did here is called static partitioning
-- And this is considered an anti-pattern
-- Next time we're going to try dynamic partitioning
