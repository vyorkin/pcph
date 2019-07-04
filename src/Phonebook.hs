module Main (main) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)

type Name = String

type PhoneNumber = String

type PhoneBook = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

main :: IO ()
main = do
  s <- new
  sequence_ [insert s ("name" ++ show n) (show n) | n <- [1.. 100000] :: [Int]]
  lookup s "name999" >>= print
  lookup s "unknown" >>= print

new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return $ PhoneBookState m

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (Map.insert name number book)
  -- ^^^
  -- The benefit is that we don’t have to wait for Map.insert to
  -- finish before we can unlock the state; as in lookup, the
  -- state is only locked very briefly. However, if we were to do
  -- many insert operations consecutively, the MVar would build up
  -- a large chain of unevaluated expressions, which could create
  -- a space leak.

insert' :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert' (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m $! Map.insert name number book

-- ^^^
-- Now we hold the lock until Map.insert has completed, but
-- there is no risk of a space leak. To get brief locking and no
-- space leaks, we need to use a trick:

insert'' :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert'' (PhoneBookState m) name number = do
  book <- takeMVar m
  let book' = Map.insert name number book
  putMVar m book'
  seq book' $ return ()

-- We’re storing an unevaluated expression in the MVar, but it
-- is evaluated immediately after the putMVar. The lock is held
-- only briefly, but now the thunk is also evaluated so we avoid
-- building up a long chain of thunks.

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return (Map.lookup name book)
  -- ^^^
  -- Note that we need to put back the state after taking it;
  -- otherwise, the state would remain locked after lookup
  -- returns.
