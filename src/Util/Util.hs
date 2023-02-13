{-# LANGUAGE OverloadedStrings #-}

module Util.Util
    (
      rmQuote,
      cnvValue,
      getNid,
      getDir,
      getCfgProp,
      parseHops,
      parseSteps,
      getMap,
      loadConn,
      loadHop,
      loadStp,
      mapFile,
      atomRead,
      parseProps,
      getStepNames,
      bufferIze,
      mkRows,
      getHopValue,
      sendBuf,
      sendVar
    ) where


import Struct.Architecture
import Control.Distributed.Process
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as L
import qualified Data.ByteString.UTF8 as U ( fromString, toString )
-- import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import           Control.Arrow                 ((&&&))
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson
import Data.Aeson.Text
import Network.Transport (EndPointAddress(EndPointAddress))
import Control.Concurrent.STM (atomically, readTMVar)
import qualified Control.Concurrent.STM as Control.Concurrent.STM.TMVar
import Data.ByteString (ByteString)
import Data.Aeson.KeyMap as K (lookup, KeyMap)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Codec.Xlsx.Util.Tabular.Imports (fromMaybe)
import qualified Data.String
import Text.Read (readMaybe)
import qualified Data.Aeson.Key as Y
import qualified Data.Aeson.Types as Y
import Data.Maybe
import Control.Concurrent.STM.TMVar (putTMVar)

-- Remove punctuation from text String.
rmQuote :: String -> String
rmQuote = Prelude.filter (not . (`elem` ("\"" :: String)))

--cnvValue :: Object -> String
cnvValue xs = rmQuote (unpack xs)

-- Get parts for Step Network ID, Build the ID and return it

getNid :: (String, StepProperty) -> H.HashMap String ConfigProperty -> NodeId
getNid s cm  =
   NodeId . EndPointAddress $ L.pack(hostName n ++ ":" ++ show (portNumber n) ++ ":0")
    where
      n = cm H.! runLoc (snd s)

getDir :: (String, StepProperty) -> H.HashMap String ConfigProperty -> String
getDir s cm  =
   mapDirectory n
    where
      n = cm H.! runLoc (snd s)

getCfgProp :: Connections -> ConfigProperty
getCfgProp = cfgProperty

mapFile :: String -> FilePath
mapFile m = "maps/trg01_csv/" ++ m ++ ".json"

getMap :: String -> IO B.ByteString
getMap m = B.readFile $ mapFile m

parseSteps :: String -> IO (Either String [Steps])
parseSteps m = fmap steps <$> (eitherDecode <$> getMap m)

parseProps :: String -> IO (Either String Property)
parseProps m = fmap property <$> (eitherDecode <$> getMap m)

parseHops :: String -> IO (Either String [Hops])
parseHops m = fmap hops <$> (eitherDecode <$> getMap m)

loadConn :: [Connections] -> H.HashMap String ConfigProperty
loadConn = H.fromList . Prelude.map (connectionName Control.Arrow.&&& cfgProperty)

loadStp :: [Steps] -> H.HashMap String StepProperty
loadStp = H.fromList . Prelude.map (stepName Control.Arrow.&&& stepProperty)  -- rewrite Step to break out stepName like Connections

loadProp :: Property -> (String, Integer)
loadProp p = (mapName p, bufferSize p)

loadHop :: [Hops] -> [HopObj]
loadHop = Prelude.map hop

getStepNames :: [(String,StepProperty)] -> [Y.Key]
getStepNames stp
  | Prelude.null stp = []
getStepNames (x:xs) = Y.fromString (fst x)  : getStepNames xs

atomRead :: Control.Concurrent.STM.TMVar.TMVar a -> IO a
atomRead = atomically . readTMVar

bufferIze :: [[String]] -> Int -> ([[ByteString]],[[String]])
bufferIze ib ix = ((map.map) U.fromString (take ix ib),drop ix ib)

mkRows :: SendPort Buf -> [[String]] -> Int -> Process ()
mkRows chan c bs = do
  let bRow = bufferIze c bs
  sendChan chan (IBuf $ BBuf (fst bRow))
  mkRows chan (snd bRow) bs


getHopValue :: Key -> KeyMap a -> a
getHopValue ky ohop = do
--   rmQuote $  show $ fromMaybe "" (K.lookup ky ohop)
    fromJust (K.lookup ky ohop)

sendBuf :: ReceivePort Buf -> SendPort Buf -> Int -> Process ()
sendBuf rcvPort chan bs = do
  (IBuf g) <- receiveChan rcvPort
  sendChan chan (IBuf $  g)
  sendBuf rcvPort chan bs

sendVar :: ReceivePort Buf -> (a, Control.Concurrent.STM.TMVar.TMVar Rq) -> String -> EBuf -> Process ()
sendVar rcvPort rqt t e = do
  (IBuf g) <- receiveChan rcvPort --get response from last step
  let m = mid e
  let og = OBuf $ (map.map) U.toString (body g) -- convert from BBuf to OBuf (Strings -> ByteStrings)
  let eb = RBuf og m
  let rq = Rq eb t []  -- build response
  liftIO . atomically $ putTMVar (snd rqt) rq -- Send results back to Service
  sendVar rcvPort rqt t e