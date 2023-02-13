-- Hade - Haskell Application Development Environment
-- Author - C.E. Petersen
-- Origin Date - 08-31-2017
-- Revision 1.0
--
--
--
-- {-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- {-# LANGUAGE TypeOperators   #-}
module Step.ReadCsv
    ( readCsv
    ) where
import Control.Distributed.Process
import Text.CSV ( parseCSV )
import Struct.Architecture
    ( Config(Config),
      Property(bufferSize),
      Prp(IProp),
      StepProperty(columns, file),
      Steps(stepName, stepProperty),
      Stp(IStep), Buf (IChan) )
import Util.Util (loadConn,getDir, mkRows)


readCsv :: Process ()
readCsv = do
  say "Got Chan"
  IStep stp <- expect
  Config cfg <- expect
  IProp p <-expect
  IChan chan <- expect
  let bs = fromIntegral $ bufferSize p
  let col = columns $ stepProperty stp 
  let inf = file $ stepProperty stp
  let c = loadConn cfg 
  let dir = getDir (stepName stp,stepProperty stp) c
  let csvFile = dir ++ "/" ++ inf
  input <- liftIO $ readFile csvFile
  let cf = parseCSV csvFile input
  either handleError (mkRows chan) cf bs

handleError :: MonadFail m => p1 -> p2 -> m a
handleError c b = fail "error parsing"

