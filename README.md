# PCPH

## Synopsis

Some notes, quotes & examples that I take while reading the PCPH (Parallel and
Concurrent Programming in Haskell book) by Simon Marlow.

# Notes

### Basics

#### `NFData`

```haskell
class NFData a where
  rnf :: a -> ()
  rnf a = a `seq` ()
```

The `rnf` name stands for "reduce to normal-form."
It fully evaluates its argument and then returns `()`.

```haskell
data Tree a = Empty | Branch (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf Empty = ()
  rnf (Branch l x r) = rnf l `seq` rnf x `seq` rnf r
```

#### `seq`, `deepseq`, `rnf`, `evaluate`, `force`

The idea is to just recursively apply `rnf` to the components of the data type,
composing the calls to `rnf` together with `seq`.

`a seq b` - Evaluate `a` to WHNF and return `b`.

`rnf a` - Evaluate `a` to NF and return `()`.

The default definition uses seq,
which is convenient for types that have no substructure:

```haskell
rnf a = a `seq` ()
```

`a deepseq b` - Evaluate `a` to NF.

```haskell
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b
```

* `force` - Turn WHNF into NF.

If the program evaluates `force x` to WHNF,
then `x` will be evaluated to NF.

```haskell
force :: NFData a => a -> a
force x = x `deepseq` x
```

CAUTION: Avoid repeated uses of `force` or `deepseq` on the same data.

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

## Eval Strategies

```haskell
type Strategy a = a -> Eval a
```

`Strategy` takes a data structure as input, traverses the structure creating
parallelism with `rpar` and rseq, and then returns the original value.

For example, here is the strategy that fully evaluates its argument:

```haskell
rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)
```

which is equivalent to:

```haskell
rdeepseq :: NFData a => Strategy a
rdeepseq x = do rseq (rnf x); return x
```

Use `rparWith` to apply a `Strategy` in parallel.

```haskell
rparWith :: Strategy a -> Strategy a
```

`r0` - Don't evaluate this component at all.

For example, the following evaluates only the
first component of both pairs in parallel.

```haskell
evalPair (evalPair rpar r0) (evalPair rpar r0)
```

`parList` evaluates all the items in parallel, setting them all off at once.
This is useful when you want to consume the entire list at once.

The `parBuffer` function has a similar type to `parList` but takes an Int argument
as a buffer size. In contrast to `parList` which eagerly creates a spark for every
list element, `parBuffer N` creates sparks for only the first `N` elements of the
list, and then creates more sparks as the result list is consumed. The effect is
that there will always be N sparks available until the end of the list is
reached.

`parBuffer` evaluates the first `n` elements, and when you consume beyond that, it
sets off the next `n`, and so on. `parBuffer` makes sense when you are going to
consume the list in chunks, starting at the beginning. Or when the list is very
large (or infinite) and you won't evaluate it all

`parBuffer` is conceptually similar to a circular buffer with a constant window
size rolling over the input and producing the output and is useful when
implementing pipeline parallelism or working with lazy streams.
