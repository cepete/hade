-- Hade - Haskell Application Development Environment
-- Author - C.E. Petersen
-- Origin Date - 08-31-2017
-- Revision 1.0
--
--
--

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Step.ReadExcel
    ( readExcel
    ) where
import Codec.Xlsx
--import Codec.Xlsx.Util.Tabular as T
import Codec.Xlsx.Util.Tabular.Json()
--import Data.Aeson as A
--import Data.String as L
import Control.Distributed.Process
import Control.Lens
import Struct.Architecture as S
import Data.ByteString.Lazy as U (readFile,ByteString,pack,unpack, zip)
--import Data.ByteString.Lazy.UTF8 as BU (fromString)
--import Data.ByteString.Conversion
--import Data.Word8
--import Text.Printf (printf)



--readExcel == errorWithoutStackTrace "You need to implement this function."
readExcel :: Process ()
readExcel = do
  say "ReadExcel"
  IStep stp <- expect
  Config cfg <- expect
  IChan chan <- expect
  ps <- liftIO j
  let buf = returnBuf stp ps
  say "Got Chan2"
  sendChan chan (IBuf $ BBuf buf)
  where
    j = do
      bs <- U.readFile "/apps/workspace/hade/maps/src03_excel/in_test_excel1.xlsx"
      --let value = toXlsx bs ^? ixSheet "Sheet1"  -- . ixCell (3,2) . cellValue . _Just
      return bs

returnBuf :: Steps -> ByteString -> [Row]
returnBuf stp bs = do
  let ws = toXlsx bs ^? ixSheet "Sheet1"
  let w = removeMaybe ws
  buildBuf 1 (Prelude.length $ columns $ stepProperty stp) (head w)

buildBuf :: Int -> Int -> Worksheet -> [Row]
buildBuf x y sht = do
  let z = _wsCells sht
  []
  
  --buildBuf x y sht :: buildBuf (x+1) y sht

--buildRow :: Int -> Int -> Worksheet -> Row
--buildRow (x:xs) (y:ys) =  toByteString y:buildRow xs ys
--buildRow []     _ = []


--getCell :: Int -> Int -> Worksheet -> Maybe CellValue
--getCell  x y wrk = do
--  let c = wrk . ixCell (x,y) . cellValue . _Just
--  removeMaybe c . head

removeMaybe :: Maybe a -> [a]
removeMaybe Nothing = []
removeMaybe (Just i) = [i]

