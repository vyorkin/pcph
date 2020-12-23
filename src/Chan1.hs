module Main (main) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)
data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))
-- ~
data Item' a = Item' a (MVar (Item' a))
data Chan' a = Chan'
  (MVar (MVar (Item' a))) --  readVar
  (MVar (MVar (Item' a))) -- writeVar

main :: IO ()
main = return ()

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return $ Chan readVar writeVar

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole) -- (1)
  putMVar writeVar newHole           -- (2)

-- [b] -> [x] -> ... -> [e]
--                       ^-- oldHole
--                       |
--                   [Item val newHole]   -- (1)
--                                ^
--                               [e]      -- (2)

readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar       --     R-lock
  Item val tail <- takeMVar stream -- (1) lock stream (forever, take and never put back)
  putMVar readVar tail             -- (2) R-unlock
  return val

--      [b] -> [x] -> ... -> [e]
--       ^
-- (Item val tail)                     -- (1)
--             ^
--            [b] -> [x] -> ... -> [e] -- (2)

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- readMVar writeVar -- W-lock + W-unlock
  newReadVar <- newMVar hole
  return $ Chan newReadVar writeVar

readMVar :: MVar a -> IO a
readMVar m = do
  a <- takeMVar m
  putMVar m a
  return a

-- original:  [b(s) = e] |> [b (s) = e] |> [lock(b(s) = e]
--                       |>     ^       |> deadlock on [s]
-- duplicate:            |> [b'(s) = e] |> [lock(b'(s))]
------------------------------------------------------------------
--                       |  dup         |  readChan

-- `readChan` should use `readMVar` instead
-- of `takeMVar` to read the stream

readChan' :: Chan a -> IO a
readChan' (Chan readVar _) = do
  stream <- takeMVar readVar       -- R-lock
  Item val tail <- readMVar stream -- lock & unlock stream
  putMVar readVar tail             -- R-unlock
  return val

-- ^^^ `readVar` returns the Item back to the Stream, where it
-- can be read by any duplicate channels created by dupChan'.

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _) val = do
  newReadEnd <- newEmptyMVar
  readEnd <- takeMVar readVar           -- R-lock
  putMVar newReadEnd (Item val readEnd)
  putMVar readVar newReadEnd            -- R-unlock
