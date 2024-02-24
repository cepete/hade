{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Struct.Struct where

import Data.Aeson as A
import GHC.Generics
import Data.Binary.Orphans
import Data.HashMap.Strict
--import Network.Google.Data.Bytes
import Data.Typeable
import Control.Concurrent.STM
import Control.Distributed.Process
import Struct.Architecture
import Servant
import Data.ByteString.Lazy
--data Step = Step Integer deriving (Eq, Show, Typeable, Generic)
data Act = Act Integer deriving (Eq, Show, Typeable, Generic)
--data IBuf = IBuf
--  { id :: Integer
--  , buf :: String
--  } deriving (Show, Eq, Generic, Typeable)

--instance Binary Step
instance Binary Act

type Command = String
type SMap = JSON
type Parm = []
type PReq = (SMap,Parm String)



data RQueue = RQueue
  {
    slot :: PReq
  }

--data Obj = forall a. (Show a) => Obj a

--xs :: [Obj]
--xs = [Obj 1, Obj "foo", Obj 'c']

--doShow :: [Obj] -> String
--doShow [] = ""
--doShow ((Obj x):xs) = show x ++ doShow xs

--instance Generic Obj
--instance Binary Obj

instance Binary Bytes

data BBuf = BBuf {body :: ByteString} deriving (Show,Generic)
data EBuf = EBuf {  bufr :: BBuf
                  , mid ::  Int} deriving (Show,Generic)

data StProc = StProc {  stepName :: String
                  , prid ::  ProcessId} deriving (Show,Generic)

data Rq = Rq {ebfr :: EBuf
                  , trans :: String
                  , pms :: [String]
                  } deriving (Show,Generic)


--type StpObj = HashMap String StepProperty
--type HopObj = [Object]

type StpObj = [Object]

data Buf = IChan (SendPort Buf) | IHash SConfig | IBuf BBuf deriving (Show,Generic,Typeable)
instance Binary EBuf
instance Binary Rq
instance Binary BBuf
instance Binary Buf

instance FromJSON EBuf
instance FromJSON Rq
instance FromJSON BBuf
instance ToJSON EBuf
instance ToJSON Rq
instance ToJSON BBuf

type Req = (TMVar Rq,TMVar Rq)
