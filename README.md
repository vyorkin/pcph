# PCPH

Some notes & examples while reading the PCPH (Parallel and Concurrent
Programming in Haskell book) by Simon Marlow.

# Notes

### `Control.Parallel.Strategies`

`rpar` - Argument could be evaluated in parallel
`rseq` - Evaluate argument and wait for the result

In both cases evaluation is to WHNF.

Patterns:

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


## `NFData`

```haskell
class NFData a where
  rnf :: a -> ()
  rnf a = a `seq` ()
```

The `rnf` name stands for “reduce to normal-form.”
It fully evaluates its argument and then returns `()`.

```haskell
data Tree a = Empty | Branch (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf Empty = ()
  rnf (Branch l x r) = rnf l `seq` rnf x `seq` rnf r
```

The idea is to just recursively apply `rnf` to the components of the data type,
composing the calls to rnf together with `seq`.
