# PCPH

Some notes & examples while reading the PCPH (Parallel and Concurrent
Programming in Haskell book) by Simon Marlow.

# Notes

### Control.Parallel.Strategies

1. `rpar` + `rpar`

```haskell
fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rpar (f y)
  return (a, b)
```

Use when you don't depend on the results of either computation.

2. `rpar` + `rseq` + `rseq`

```haskell
fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rseq (f y)
  void $ rseq a
  return (a, b)
```

Use when you need the results of
one of the operations in order to continue.

3. `rpar` + `rseq`

```haskell
fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rseq (f y)
  return (a, b)
```

Unlikely to be useful. We rarely know in advance which of the two computations
is the longest one.
