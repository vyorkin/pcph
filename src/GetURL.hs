-- Simple wrapper around HTTP, allowing proxy use

module GetURL (getURL) where

import Network.HTTP.Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

getURL :: String -> IO ByteString
getURL url = L.toStrict <$> simpleHttp url
