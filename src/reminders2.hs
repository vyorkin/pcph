module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Text.Printf (printf)

main :: IO ()
main = loop
  where
    loop :: IO ()
    loop = do
      s <- getLine
      if s == "exit"
      then return ()
      else void (forkIO (setReminder s)) >> loop

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Next reminder in %d seconds\n" t
  void $ threadDelay $ 10 ^ 6 * t
  printf "%d seconds is up! BING!\BEL\n" t
