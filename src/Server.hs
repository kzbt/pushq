{-# LANGUAGE OverloadedStrings #-}

module Server ( app) where

import Data.ByteString.Char8
import Data.ByteString.Builder
import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Network.Wai
import Network.Wai.Internal
import Network.HTTP.Types
import System.IO.Unsafe

msgq :: TChan String
msgq = unsafePerformIO newTChanIO

contentTypeJson = ("Content-Type", "application/json")

app :: Application
app req respond = do
  print req
  case (requestMethod req, rawPathInfo req) of
    ("GET", "/msg") -> pushMsg respond
    ("POST", "/msg") -> do
      body <- getRequestBodyChunk req
      enqueueMsg (unpack body) respond
    (_, _) -> unsupported respond

pushMsg respond = respond $ responseStream status200 [contentTypeJson] pushfn

pushfn write flush = forever $ do
    msg <- atomically $ readTChan msgq
    write $ stringUtf8 $ msg ++ "\n"
    flush

enqueueMsg msg respond = do
  atomically $ writeTChan msgq msg
  respond $ responseLBS status200 [] "Queued for delivery"


unsupported respond = respond $ responseLBS status405 [] "Unsupported"
