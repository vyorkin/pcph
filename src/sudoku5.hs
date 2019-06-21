module Main (main) where

import System.Environment (getArgs)
import Sudoku (solve)
import Control.Monad (void)
import Data.Maybe (isJust)
import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Parallel.Strategies (Strategy, Eval, runEval, rpar, rseq, rparWith, using)

-- SPARKS: 1000(14 converted, 0 overflowed, 0 dud, 986 GC'd, 0 fizzled)

-- If you see that a large number of sparks are GC’d, it’s a good indication
-- that sparks are being removed from the spark pool before they can be used for
-- parallelism. Unless you are using speculation, a non-zero figure for GC’d
-- sparks is probably a bad sign.

main :: IO ()
main = do
  [f]  <- getArgs
  file <- readFile f
  let
    puzzles = lines file
    solutions = solve <$> puzzles `using` badParList rseq

  print (length (filter isJust solutions))

badParList :: Strategy a -> Strategy [a]
badParList strat as = do
  go as
  return as
  where
    go [] = return ()
    go (x:xs) = void $ rparWith strat x >> go xs
