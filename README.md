# PCPH

## Synopsis

Some notes, quotes & examples that I take while reading the PCPH (Parallel and
Concurrent Programming in Haskell book) by Simon Marlow.

# Notes

### Intro

`:sprint` – Prints a value without forcing its evaluation.

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

The idea is to just recursively apply `rnf` to the components of
the data type, composing the calls to `rnf` together with `seq`.

#### `seq`, `deepseq`, `rnf`, `evaluate`, `force`

* `a seq b` - Evaluate `a` to WHNF and return `b`
* `rnf a` - Evaluate `a` to NF and return `()`

which is convenient for types that have no substructure:

```haskell
rnf a = a `seq` ()
```

`a deepseq b` - Evaluate `a` to NF.

```haskell
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b
```

`force` - Turn WHNF into NF.

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
  b <- rseq (f y) -- wait for (f y)
  void $ rseq a   -- wait for (f x)
  return (a, b)
```

Use when you need the results of **both** operations in order
to continue.

3. `rpar` + `rseq`

```haskell
fn :: Int -> Int -> (Int, Int)
fn x y = runEval $ do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rseq (f y)
  return (a, b)
```

Unlikely to be useful. We rarely know in advance which of the
two computations is the longest one.

4. `rpar` + `rpar` + `rseq` + `rseq`

```haskell
fn :: Eval (Integer, Integer)
fn = do
  let f = (+ 1)
  a <- rpar (f x)
  b <- rpar (f y)
  void $ rseq a
  void $ rseq b
  return (x, y)
```

Same as 2. but looks better (because of `rpar - rseq` symmetry!), so
this one is preferable.

Summary: if you don't depend on the results of either operation
and want to generate more parallelism -- use `rpar + rpar`.
Otherwise, if we've generated all the parallelism we can, or we
need the results of one of the operations in order to continue
-- use `rpar + rseq + rseq` (or `rpar + rpar + rseq + rseq`,
which is more "symmetric").

**CAUTION**: Not evaluating deeply enough is a common mistake
when using `rpar`.

General rules:

**Avoid static partitioning**: Don't try to partition the work
into a known fixed amount of chunks. These chunks rarely contain
an equal amount of work, so there will be some imbalance between
HEC's, leading to a loss of speedup.

Parallelism is limited to the number of chunks, so use **dynamic
partitioning**, which means supplying just enough tasks (sparks)
by calling `rpar` (the argument to `rpar` is called _spark_) often
enough so that GHC can balance the work evenly.

The runtime collects sparks in a pool and uses it as a source of
work when there are spare processors available. Sparks are cheap
(just a pointer to the expression).

**Avoid creating too many sparks**: There is some overhead per
chunk in creating the spark and arranging to run it on another
processor. The smaller chunks are, the more significant becomes
the overhead.

## Eval Strategies

`Strategy` separates the algorithm from the parallelism. It
takes a data structure as input, traverses the structure
creating parallelism with `rpar` and `rseq`, and then returns
the original value.


```haskell
type Strategy a = a -> Eval a
```

Parallelism is expressed using the `Eval` monad.

```haskell
newtype Eval a = Eval { unEval_ :: IO a }
  deriving (Functor, Applicative, Monad)
```

```haskell
runEval :: Eval a -> a
```

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

`parList` evaluates all the items in parallel, setting them all
off at once. This is useful when you want to consume the entire
list at once.

The `parBuffer` function has a similar type to `parList` but
takes an `Int` argument as a buffer size. In contrast to
`parList` which eagerly creates a spark for every list element,
`parBuffer N` creates sparks for only the first `N` elements of
the list, and then creates more sparks as the result list is
consumed. The effect is that there will always be `N` sparks
available until the end of the list is reached.

`parBuffer` evaluates the first `n` elements, and when you
consume beyond that, it sets off the next `n`, and so on.
`parBuffer` makes sense when you are going to consume the list
in chunks, starting at the beginning. Or when the list is very
large (or infinite) and you won't evaluate it all

`parBuffer` is conceptually similar to a circular buffer with a constant window
size rolling over the input and producing the output and is useful when
implementing pipeline parallelism or working with lazy streams.

## The `Par` monad

`Par` monad is particularly suited to expressing dataflow
networks.

```haskell
newtype Par a
instance Applicative Par
instance Monad Par

runPar :: Par a -> a
```

Forks a computation to happen in parallel:

```haskell
fork :: Par () -> Par ()
```

Values can be passed between `Par` computations using the
`IVar` type and its operations:

```haskell
new :: Par (IVar a)
put :: NFData a => IVar a -> a -> Par ()
get :: IVar a -> Par a
```

Think of an `IVar` as a box that starts empty. The `put`
operation stores a value in the box, and `get` reads the value.
If the `get` operation finds the box empty, then it waits until
the box is filled by a `put`.

* `get x` means "wait `x` to finish"
* `mapM get xs` means "wait for all `xs` to finish"

Once filled, the box stays full; the `get` operation doesn’t
remove the value from the box. It is an error to call `put` more
than once on the same `IVar`.

The `put` function calls `deepseq` on the value it puts in the
`IVar`, which is why its type has an `NFData` constraint.

`IVar` and `MVar` are similar, the main difference being that an
`IVar` can be written only once.

```haskell
newtype IVar a = IVar (IORef (IVarContents a))
```

**CAUTION**: Never return `IVar a` from `runPar`.

`spawn` forks a computation in parallel and returns an `IVar`
that can be used to wait for the result. Basically it is `fork +
put`.

```haskell
spawn :: NFData a => Par a -> Par (IVar a)
```

Parallel map consists of calling `spawn` to apply the function
to each element of the list and then waiting for all the
results.

```haskell
parMap :: NFData b => (a -> b) -> [a] -> Par [b]
```

```haskell
parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
```

Note that in `parMapM` the function argument, `f`, returns its
result in the `Par` monad; this means that `f` itself can create
further parallelism using `fork` and the other `Par` operations.

`parMapM` and `parMap` wait for all the results before
returning. Depending on the context, this may or may not be the
most useful behavior. If you don’t want to wait for the results,
then you could always just use `mapM (spawn . f)`, which returns
a list of `IVars`.

The `put_` operation evaluates the value to WHNF only. Replacing
`put` with `put_` can save some time if you know that the
argument is already fully evaluated.

```haskell
put_ :: IVar a -> a -> Par ()
```

About `ParVis`:

* https://www.youtube.com/watch?v=lJ12sqGHctU
* http://www.cse.chalmers.se/~patrikj/papers/Algehed_Jansson_VisPar_preprint_2017-06-09.pdf

## Repa (REgular PArallel arrays)

We can't use `Strategy`'s to parallelize large-scale array
computations, becuase they require operations over unboxed
arrays. Similarly, `Par` doesn't work well here either, because
in `Par` the data is passed in `IVar`'s.

**Repa** provides a range of efficient operations for creating
arrays and operating on arrays in parallel.

```haskell
data Array r sh e
```

* `e` - type of the elements
* `r` - representation type
* `sh` - shape of the array (number of dimensions)

Shapes are built out of two type constructors, `Z` and `:.`

```haskell
data Z = Z
data tail :. head = tail :. head
```

* `Z` - shape of an array with no dimensions (scalar)
* `Z :. Int` - array with a single dimension indexed by `Int`
* `Z :. Int :. Int` - 2-dimensional array

The `Z` and `:.` symbols are both type constructors and value
constructors. E.g. `Z :. 3` can be either the shape of 3-element
vectors, or the index of the 4-th element of a vector.

A few handy type synonyms:

```haskell
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
```

```haskell
fromListUnboxed :: (Shape sh, Unbox a) => sh -> [a] -> Array U sh a
```

* `U` means "unboxed"

## Concurrency

The `MVar` is a fundamental building block that generalizes many
different communication and synchronization patterns.

```haskell
data MVar a

newEmptyMVar :: IO (MVar a)
newMVar :: a -> IO (MVar a)
takeMVar :: MVar a -> IO a
putMVar :: MVar a -> a -> IO ()
```

* `MVar` - Is a box (empty/full).
* `newMVar` - Creates a new **full** box (with a value passed as
  its arguments).
* `takeMVar` - Removes the values from a **full** `MVar` and
  returns it. Blocks (waits) if the `MVar` is currently empty.
* `putMVar` - Puts a value into `MVar` but blocks if the `MVar`
   is already full.

`MVar` is a:
* One-place channel (box) for passing messages between threads.
* Container for shared mutable state.

Examples:

```haskell
main = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r
```

```haskell
main = do
  m <- newEmptyMVar
  forkIO $ do
    putMVar m 'x'
    putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
```

Deadlock detection:

```haskell
main = do
  m <- newEmptyMVar
  takeMVar m
```

The run time detects this and throws `BlockedIndefinitelyOnMVar`
exception:

```
thread blocked indefinitely in an MVar operation
```

`MVar` provides the combination of a lock and a mutable variable
in Haskell. To **acquire the lock**, we **take** the `MVar`,
whereas, to update the variable and **release the lock**, we
**put** the `MVar`.

 We can take any pure immutable data structure such as `Map` and
 turn it into mutable shared state by simply wrapping it in an
 `MVar`.

## Exceptions

```haskell
throw :: Exception e => e -> a
```

```haskell
class (Typeable e, Show e) => Exception e where
  -- ...
```

```haskell
newtype ErrorCall = ErrorCall String
    deriving (Typeable)

instance Show ErrorCall where { ... }

instance Exception ErrorCall
```

```haskell
throw (ErrorCall "oops!")
```

```haskell
error :: String -> a
error s = throw (ErrorCall s)
```

Exceptions in Haskell can be caught, _but only in the `IO` monad_:

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

The behavior is as follows: the `IO` operation in the first
argument is performed, and if it throws an exception of the type
expected by the handler, `catch` executes the handler, passing
it the exception value that was thrown. So a call to `catch`
catches only exceptions of a particular type, determined by the
argument type of the exception handler.


Similar to `catch` but returns an `Either`:

```haskell
try :: Exception e => IO a -> IO (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))
```

Takes an exception predicate to select which exceptions are caught:

```haskell
tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
```

```haskell
handle :: Exception e => (e -> IO a) -> IO a -> IO a
```

It is often useful to be able to perform some operation if an
exception is raised and then re-throw the exception:

```haskell
onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do $
  _ <- what
  throwIO (e :: SomeException)
```

`throwIO` guarantees strict ordering with respect to other IO
operations, whereas `throw` does not.

```haskell
throwIO :: Exception e => e -> IO a
```

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after during = do
  a <- before
  c <- during a `onException` after a
  after a
  return c
```

```haskell
finally :: IO a -> IO b -> IO a
finally io after = do
  io `onException` after
  after
```

Every `forkIO` thread gets a default exception handler, that
prints the exception to `stderr`, and then the thread
terminates.

## Async Exceptions

To initiate an asynchronous exception:

```haskell
throwTo :: Exception e => ThreadId -> e -> IO ()
```

```haskell

```

```haskell

```
