module Main (main) where

import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Monad (replicateM_, void)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  void $ forkIO $ replicateM_ 10000 (putChar 'A')
  replicateM_ 10000 (putChar 'B')
