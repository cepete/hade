-- Hade - Haskell Application Development Environment
-- Author - C.E. Petersen
-- Origin Date - 08-31-2017
-- Revision 1.0
--
--
--
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoOverloadedStrings #-}
module Actor.MasterActor where

import           Actor.Master
--import           Control.Concurrent
--import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
--import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad
--import           Control.Monad.Trans
import           Data.Aeson                                         as A
--import           Data.Aeson.Types
import qualified Data.ByteString.Lazy                               as B
--import           Data.ByteString.Lazy.UTF8                          as U
--import           Data.Int
--import           Data.List
--import           Data.String
--import           Data.Functor
--import           Data.Text as T
--import           Text.Printf
--import           GHC.Generics
--import           Server.Service
--import           Step.ReadExcel
--import           Step.ReadCsv
import           Struct.Architecture
--import           System.Environment                                 (getArgs)
--

masterActor :: Backend -> Req -> [NodeId] -> Process ()
masterActor backend request slaves = do
  --  parseCfg
  c <- liftIO parseCfg
  let con = case c of
              Left err -> fail err
              Right j -> parseConn j
  forever $
    master slaves con request

--atomRead = atomically . readTVar

jsonFile :: FilePath
jsonFile = "config/config.json"

getCfg :: IO B.ByteString
getCfg = B.readFile jsonFile

parseCfg :: IO (Either String Config)
parseCfg = fmap config <$> (eitherDecode <$> getCfg)

parseConn :: Config -> [Connections]
parseConn c = connections c
