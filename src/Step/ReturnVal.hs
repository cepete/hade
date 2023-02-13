-- {-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators   #-}
module Step.ReturnVal
    ( returnVal
    ) where
import Control.Distributed.Process as P
import Struct.Architecture
import Control.Monad()
import Util.Util


returnVal :: Process ()
returnVal = do
  say "ReturnVal"
  IStep stp <- expect
  say "IStepR"
  Config cfg <- expect
  say "ConfigR"
  IProp p <-expect
  say "IPropR"
  let bs = fromIntegral $ bufferSize p
  pid <- expect
  IChan chan <- expect
  (sendPort, rcvPort) <- P.newChan
  P.send pid (IChan sendPort)
  sendBuf rcvPort chan bs
  --say $ printf "recieved %s" (show g)
