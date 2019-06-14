module Main (main) where

import Control.DeepSeq (NFData, rnf, force)
import Control.Parallel.Strategies (Strategy, using, rpar, rseq, rparWith, r0)

-- type Strategy a = a -> Eval a

main :: IO ()
main = print pair where
  pair = (fib 35, fib 36) `using` parPair

parPair :: Strategy (a, b)
parPair = evalPair rpar rpar

rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)

parPairWith :: Strategy a -> Strategy b -> Strategy (a, b)
parPairWith sa sb = evalPair (rparWith sa) (rparWith sb)

-- parPairWith rdeepseq rdeepseq :: (NFData a, NFData b) => Strategy (a, b)

foo :: Strategy ((a1, b1), (a2, b2))
foo = evalPair (evalPair rpar r0) (evalPair rpar r0)

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
