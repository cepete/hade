{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators   #-}
module Actor.Master
    ( master
    ) where

import Control.Distributed.Process as P
import Control.Distributed.Process.Closure
import Step.ReadExcel
import Step.ReadCsv
import Control.Concurrent
import Control.Concurrent.STM
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad
import Text.Printf
import Struct.Struct
import Struct.Architecture
--import Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Lazy as H
import Data.Aeson as A
--import Data.List.Split
--

master :: [NodeId] -> [Connections] -> Req -> Process ()
master slaves q rqt = do
  let cm = loadConn q
  s <- liftIO $ parseSteps
  let st = case s of
              Left err -> fail err
              Right s -> s
  h <- liftIO $ parseHops
  let hp = case h of
              Left err -> fail err
              Right h -> h
  ps <- forM slaves $ \nid -> do
          say $ printf "Steps: %s" (show st)
          say $ printf "Hops: %s" (show h)
--          say $ printf "ConnName: %s" (show $ H.lookup "src01_csv" cm)
          say $ printf "spawning on %s" (show nid)
--          map (\st -> launchProc(cm,st) s
          (sendPort, rcvPort) <- P.newChan
          send pid (IChan sendPort)
          (BBuf r) <- liftIO (atomRead (fst rqt))
          let sc = empty
          send pid (IHash sc)
          (IBuf g) <- receiveChan rcvPort
          liftIO . atomically $ writeTVar (snd rqt) g
  liftIO $ threadDelay 100000


atomRead = atomically . readTVar


--getStepProps :: Step -> Object
--getStepProps b = stepproperty b

getCfgProp :: Connections -> Object
getCfgProp = cfgProperty

mapFile :: FilePath
mapFile = "maps/trg01_csv/map_si_trg01_csv1.json"

getMap :: IO B.ByteString
getMap = B.readFile mapFile

parseSteps :: IO (Either String [Steps])
parseSteps = fmap steps <$> (eitherDecode <$> getMap)

parseHops :: IO (Either String [Hops])
parseHops = fmap hops <$> (eitherDecode <$> getMap)

loadConn :: [Connections] -> HashMap String Object
loadConn = H.fromList . Prelude.map (\c -> (connectionName c, cfgProperty c))

launchProc :: [Connections] -> Steps -> ProcessId
launchProc cn st = do
  pid <- spawn nid $(mkStaticClosure 'readExcel)
  sid <- spawn nid $(mkStaticClosure 'readCsv)
  return sid
