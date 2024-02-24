{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_hade (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/apps/workspace/hade/.stack-work/install/x86_64-linux-tinfo6/6a72cf9ddea6678249c996089d0a50310eacf714c6d5722d18f5038210166f61/9.0.2/bin"
libdir     = "/apps/workspace/hade/.stack-work/install/x86_64-linux-tinfo6/6a72cf9ddea6678249c996089d0a50310eacf714c6d5722d18f5038210166f61/9.0.2/lib/x86_64-linux-ghc-9.0.2/hade-0.1.0.0-8MxbsUpwRIJC1P7l9zc0R3-hade-client"
dynlibdir  = "/apps/workspace/hade/.stack-work/install/x86_64-linux-tinfo6/6a72cf9ddea6678249c996089d0a50310eacf714c6d5722d18f5038210166f61/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/apps/workspace/hade/.stack-work/install/x86_64-linux-tinfo6/6a72cf9ddea6678249c996089d0a50310eacf714c6d5722d18f5038210166f61/9.0.2/share/x86_64-linux-ghc-9.0.2/hade-0.1.0.0"
libexecdir = "/apps/workspace/hade/.stack-work/install/x86_64-linux-tinfo6/6a72cf9ddea6678249c996089d0a50310eacf714c6d5722d18f5038210166f61/9.0.2/libexec/x86_64-linux-ghc-9.0.2/hade-0.1.0.0"
sysconfdir = "/apps/workspace/hade/.stack-work/install/x86_64-linux-tinfo6/6a72cf9ddea6678249c996089d0a50310eacf714c6d5722d18f5038210166f61/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hade_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hade_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hade_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hade_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hade_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hade_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
