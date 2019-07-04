module Main (main) where

import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Text.Printf (printf)

main :: IO ()
main = forever $ do
  s <- getLine
  forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Next reminder in %d seconds\n" t
  void $ threadDelay $ 10 ^ 6 * t
  printf "%d seconds is up! BING!\BEL\n" t
