-- Hade - Haskell Application Development Environment
-- Author - C.E. Petersen
-- Origin Date - 08-31-2017
-- Revision 1.0
--
--
--
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Server.Service
  (
    startService
  )
  where

import Struct.Architecture ( Req, Rq(Rq), EBuf(EBuf), BBuf )
import Control.Distributed.Process as P ( say, liftIO, Process )
import Control.Concurrent.STM
    ( TMVar,
      atomically,
      newTVar,
      readTVar,
      writeTVar,
      takeTMVar,
      tryPutTMVar,
      TVar )
import Network.Wai.Handler.Warp ( run )
import Network.Wai ( Application )
import Data.ByteString.Lazy.UTF8 as U ( ByteString, toString )
import Servant as S
    ( Proxy(..),
      JSON,
      serve,
      QueryParam,
      ReqBody,
      type (:>),
      Post,
      Server,
      Handler )

type API = "tran" :> QueryParam "mapname" String :> ReqBody '[JSON] BBuf :> Post '[JSON] Rq
--type API = "map" :> ReqBody '[JSON] BBuf :> Post '[JSON] BBuf


--  No command
server :: Req -> TVar Int ->  Server API
server r s = tran
  where
    tran ::  Maybe String -> BBuf -> S.Handler Rq
     -- Maybe String -> BBuf -> ExceptT ServantErr IO Rq
    tran (Just m) b = do
      h <- liftIO $ atomReadT s
      let eb = EBuf b h
      let nb = Rq eb m []
      liftIO $ atomically (tryPutTMVar (fst r) nb)
      liftIO $ readResult (snd r)

api :: Proxy API
api = Proxy

app1 :: Req -> TVar Int -> Application
app1 r s = serve api (server r s)

startService :: Req -> IO ()
startService rqt = do
  sq <- atomically (newTVar 0)
  run 8080 (app1 rqt sq)

atomRead :: TMVar a -> IO a
atomRead = atomically . takeTMVar

atomReadT :: TVar a -> IO a
atomReadT = atomically . readTVar

dispVar :: Show a => TMVar a -> IO ()
dispVar x = atomRead x >>= print

appV :: (a -> a) -> TVar a -> IO ()
appV fn x = atomically $ readTVar x >>= writeTVar x . fn

putJ :: ByteString -> Process ()
putJ j = say $ "Got Chan2" ++ U.toString j

readResult r = atomRead r ++ readResult r