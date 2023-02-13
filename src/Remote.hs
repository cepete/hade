module Remote
    (
      __remoteTable
      , readCsv
      , readCsv__static
      , readExcel
      , readExcel__static
      , returnVal
      , returnVal__static
    ) where
import Step.ReadExcel
import Step.ReadCsv
import Step.ReturnVal
import Control.Distributed.Process.Closure

remotable ['readExcel, 'readCsv, 'returnVal]
