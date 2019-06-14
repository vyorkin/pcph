module Main (main) where

import Control.Monad (void)
import Control.Parallel.Strategies (Strategy, Eval, runEval, using, rpar)

-- type Strategy a = a -> Eval a

main :: IO ()
main = print pair
  where pair = (fib 35, fib 36) `using` parPair
  -- where pair = runEval (parPair (fib 35, fib 36))

parPair :: Strategy (a, b)
parPair (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
