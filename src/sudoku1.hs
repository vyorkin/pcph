module Main (main) where

import System.Environment (getArgs)
import Sudoku (solve)
import Data.Maybe (isJust)

main :: IO ()
main = do
  [f]  <- getArgs                          -- <1>
  file <- readFile f                       -- <2>
  let puzzles   = lines file               -- <3>
      solutions = map solve puzzles        -- <4>
  print (length (filter isJust solutions)) -- <5>
