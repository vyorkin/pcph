module Main (main) where

import Control.Parallel.Strategies (rpar, rseq, withStrategy, parMap, parBuffer)

main :: IO ()
main = do
  let
    range = [0..10000000]
    -- x :: Applicative f => f Int
    -- x = Just 5

    -- f :: w -> b
    -- f x = _todo

    -- g :: w -> b
    -- g x = f <*> x

 -- xs = take 5
 --    . parMap rseq (+1)
 --    $ range

    xs = take 5
       . map (+1)
       . withStrategy (parBuffer 1000 rseq)
       $ range

  print (xs :: [Integer])
