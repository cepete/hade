-- Hade - Haskell Application Development Environment
-- Author - C.E. Petersen
-- Origin Date - 08-31-2017
-- Revision 1.0
--
--
--
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Struct.Architecture where

import Data.Aeson
import GHC.Generics
import Data.Binary ( Binary)
import Data.Typeable ( Typeable )
import Data.HashMap.Internal.Strict (HashMap)
import Data.Aeson.KeyMap (KeyMap)
import Control.Distributed.Process (SendPort)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import Data.Aeson as A (FromJSON,ToJSON, Value (String), toJSON )
import Control.Monad
import Control.Concurrent.STM (TMVar)
import Servant (JSON)

data Connections = Connections { connectionName :: String
                    , cfgProperty  :: ConfigProperty
                   } deriving (Show, Read, Generic)

data ConfigProperty = ConfigProperty {envName :: String
                    , hostName :: String
                    , portNumber :: Integer
                    , mapDirectory :: String
                    } deriving (Eq,  Read, Show, Generic)

newtype SConfig = SConfig [(String,ConfigProperty)] deriving (Show,Read,Generic)

newtype Config = Config {connections :: [Connections]} deriving (Show, Read, Generic)

newtype Cfg = Cfg { config :: Config} deriving (Show, Read, Generic)



instance Binary Connections

instance Binary Config
instance Binary SConfig

instance FromJSON Cfg
instance FromJSON Config
instance FromJSON Connections
instance FromJSON ConfigProperty
instance ToJSON Cfg
instance ToJSON Config
instance ToJSON Connections


data StepProperty = StepProperty {stepType :: String
                                  , runLoc :: String
                                  , file :: String
                                  , columns :: [Columns]
                                 } deriving (Eq, Read, Show, Generic)

data Property = Property {mapName :: String
                          , bufferSize :: Integer
                          } deriving (Eq,  Read, Show, Generic)

instance Binary Property
instance Binary StepProperty
instance Binary ConfigProperty
instance Binary Steps

newtype Prp = IProp Property deriving (Read, Show, Generic,Typeable)

newtype Stp = IStep Steps deriving (Read, Show, Generic, Typeable)

instance Binary Prp
instance Binary Stp


data Steps = Steps { stepName :: String
                      , stepProperty :: StepProperty
                         } deriving (Read,Show, Generic)

--data Prop = Prop {propName :: String
--                  , propVal :: String
--                } deriving Show

data Columns = Columns { columnPos :: Integer
                      , columnName :: String
                      , dataType :: String
                      , length :: Integer
                      , decimalPos :: Maybe Integer
                    } deriving (Eq,Read,Show, Generic)

instance Binary Columns

newtype Act = Act Integer deriving (Eq, Show, Typeable, Generic)

instance Binary Act

-- Generalized to any MonadPlus instance, not just Either String
textToByteString :: MonadPlus m =>  T.Text -> m ByteString
textToByteString x = case B64.decode (E.encodeUtf8 x) of
                     Left _ -> mzero
                     Right bs -> pure bs

instance A.FromJSON ByteString where
  parseJSON (A.String x) = textToByteString x
  parseJSON _ = mzero


byteStringToText :: ByteString -> T.Text
byteStringToText = E.decodeUtf8 . B64.encode

instance A.ToJSON ByteString where
  toJSON = A.toJSON . byteStringToText

type Command = String
type SMap = JSON
type Parm = []
type PReq = (SMap,Parm String)

type BField = ByteString

type Row = [BField]

type Rows = [Row]

type OField = String 
type ORow = [OField]
type ORows = [ORow]

newtype RQueue = RQueue
  {
    slot :: PReq
  }

newtype BBuf = BBuf {body :: Rows} deriving (Show,Generic)
newtype OBuf = OBuf {obody :: ORows} deriving (Show,Generic)

data EBuf = EBuf {  bufr :: BBuf
                  , mid ::  Int} |
            RBuf {  obufr :: OBuf
                  , mid ::  Int} deriving (Show,Generic)

data Rq = Rq {ebfr :: EBuf
                  , trans :: String
                  , pms :: [String]
                  } deriving (Show,Generic)

type StpObj = HashMap String StepProperty
type HopObj = KeyMap Key

data Buf = IChan (SendPort Buf) | IHash SConfig | IBuf BBuf deriving (Show,Generic,Typeable)

data Trans = Trans {property :: Property
               , steps :: [Steps]
               , hops :: [Hops]
               } deriving (Show, Generic)

data Hops = Hops { hop :: HopObj } deriving (Show, Generic)

instance Binary OBuf
instance Binary EBuf
instance Binary Rq
instance Binary BBuf
instance Binary Buf

instance FromJSON OBuf
instance FromJSON Trans
instance FromJSON Hops
instance FromJSON EBuf
instance FromJSON Rq
instance FromJSON BBuf
instance ToJSON Trans
instance ToJSON Hops
instance ToJSON OBuf
instance ToJSON EBuf
instance ToJSON Rq
instance ToJSON BBuf

type Req = (TMVar Rq,TMVar Rq)

instance ToJSON Steps
instance ToJSON Columns
instance ToJSON Property
instance ToJSON StepProperty
instance ToJSON ConfigProperty
instance FromJSON Steps
instance FromJSON Columns
instance FromJSON Property
instance FromJSON StepProperty
