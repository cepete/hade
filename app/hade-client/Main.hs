{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
import System.Environment (getArgs)
import Remote
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, startMaster, startSlave)
import Control.Distributed.Process.Node
import Control.Monad (forM_,forever)
import Control.Concurrent (threadDelay)
import Network.Transport.TCP
--import Network.Transport.TCP
import Actor.Master

main :: IO ()
main = do
  [host,port] <- getArgs
--  let rtbl = Step.ReadExcel.__remoteTable initRemoteTable
  let ctbl = Remote.__remoteTable initRemoteTable
  backend <- initializeBackend host port ctbl
  startSlave backend
