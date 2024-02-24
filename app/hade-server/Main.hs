--{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

import System.Environment (getArgs)
import Control.Concurrent.STM ( atomically, newEmptyTMVar )
--import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, startMaster)
--import Control.Distributed.Process
import Control.Distributed.Process.Node ( initRemoteTable )
--import Network.Transport.TCP
--import Actor.Master
import Actor.MasterActor ( masterActor )
import Control.Concurrent ( forkIO, ThreadId )
import Server.Service ( startService )
import Remote ( __remoteTable )
import Struct.Architecture ( Req )
import Control.Distributed.Process
--import Data.ByteString.Char8
--import Util.Request

main :: IO ()
main = do
  [host,port] <- getArgs
  let rtbl = __remoteTable initRemoteTable
  request <- atomically $ newEmptyTMVar
  response <- atomically $ newEmptyTMVar
  let r = (request, response) :: Req
  backend <- initializeBackend host port rtbl
--  forkIO $ requestMonitor request
--  forkIO $ cacheMonitor (request,response)
  let s = forkIO $ startService r
  lg <- liftIO s
  print lg
  startMaster backend (masterActor backend r)
