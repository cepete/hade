-- Hade - Haskell Application Development Environment
-- Author - C.E. Petersen
-- Origin Date - 08-31-2017
-- Revision 1.0
--
--
--
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Actor.Master
    ( master
    ) where

import Control.Distributed.Process as P
import Control.Distributed.Process.Closure
import Network.Transport
import Remote
import Control.Concurrent.STM
import Control.Monad
import Text.Printf
import Util.Util
import Struct.Architecture as S
import Data.HashMap.Internal.Strict as S (HashMap, (!), toList, fromList)
import Data.ByteString.UTF8 as U
import qualified Data.String
import Data.Aeson.KeyMap as K (toHashMap,lookup, fromList, KeyMap)
import Codec.Xlsx.Util.Tabular.Imports (fromMaybe)
import qualified Data.Aeson.Key as Y
import Control.Distributed.Process.Internal.Types (ProcessId(ProcessId))
import System.Posix.Types
import Data.Maybe



master :: [NodeId] -> [Connections] -> Req -> Process [ProcessId]
master slaves q rqt = do
  (Rq e t p) <- liftIO $ atomRead (fst rqt)
  pr <- liftIO $ parseProps t
  pro <- either fail return pr
  s <- liftIO $ parseSteps t
  let st = case s of
              Left err -> fail err
              Right s -> s
  let ostp = loadStp st
  h <- liftIO $ parseHops t
  let hp = case h of
              Left err -> fail err
              Right h -> h
  let ohop = loadHop hp
  ps <- instSteps ostp q
  let nl = getStepNames $ toList ostp
  let ist = K.fromList $ zip nl ps
  instHops ohop ist e t rqt pro

instSteps :: StpObj -> [Connections] -> Process [ProcessId]
instSteps stp q = do
  let cm = loadConn q
  forM (toList stp) $ \sid -> do
    let nid = getNid sid cm
    let cn = fst sid
    say $ printf "StepName: %s" (show cn)
    let st = (stepType . snd) sid
    say $ printf "StepType: %s" (show st)
    pid <- spawnCls nid st
    P.send pid (IStep $ uncurry Steps sid)
    say $ printf "PID: %s" (show pid)
    P.send pid $ Config q
    return pid

spawnCls :: (Eq a, Data.String.IsString a) => NodeId -> a -> Process ProcessId
spawnCls nid s  | s == "readCsv" = spawn nid $(mkStaticClosure 'readCsv)
                | otherwise = do
                      say "Returnval"
                      spawn nid $(mkStaticClosure 'returnVal)

instHops :: [HopObj] -> KeyMap ProcessId -> EBuf -> String -> Req -> Property -> Process [ProcessId]
instHops h ist e t rqt p = do
  say (printf "PS: %s HOP: %s" (show ist) (show h))
  s <- forM  h $ \ohop -> do
    say (printf "SOURCE: %s" (show (getHopValue "hopSource" ohop)))
    let pid = fromJust (K.lookup (getHopValue "hopTarget" ohop) ist)  -- Get target step pid
    say (printf "TPID: %s" (show pid))
    let spid = fromJust (K.lookup (getHopValue "hopSource" ohop) ist) -- Get source step pid
    say (printf "SPID: %s TPID: %s" (show spid) (show pid))
    P.send spid (IProp p) -- Send step properties to step
    P.send pid (IProp p) -- Send step properties to step
    P.send pid spid  -- send source step pid to target step
    return pid
  (sendPort, rcvPort) <- P.newChan
  P.send (last s) (IChan sendPort) -- Send channel to last step to get response
  sendVar rcvPort rqt t e
  return s


