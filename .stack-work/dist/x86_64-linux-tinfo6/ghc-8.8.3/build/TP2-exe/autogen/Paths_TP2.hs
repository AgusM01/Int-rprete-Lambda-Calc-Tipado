{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP2 (
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

bindir     = "/home/martin/LCC/ALP/Interprete-Lambda-Calc-Tipado/.stack-work/install/x86_64-linux-tinfo6/c70a8f8a1954a7b02bc853c8180ce9fb6055b96a7e5aec7d0a71d9a3a863c0b6/8.8.3/bin"
libdir     = "/home/martin/LCC/ALP/Interprete-Lambda-Calc-Tipado/.stack-work/install/x86_64-linux-tinfo6/c70a8f8a1954a7b02bc853c8180ce9fb6055b96a7e5aec7d0a71d9a3a863c0b6/8.8.3/lib/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0-2c9qde79sPOLVEqyS9Uqj7-TP2-exe"
dynlibdir  = "/home/martin/LCC/ALP/Interprete-Lambda-Calc-Tipado/.stack-work/install/x86_64-linux-tinfo6/c70a8f8a1954a7b02bc853c8180ce9fb6055b96a7e5aec7d0a71d9a3a863c0b6/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/martin/LCC/ALP/Interprete-Lambda-Calc-Tipado/.stack-work/install/x86_64-linux-tinfo6/c70a8f8a1954a7b02bc853c8180ce9fb6055b96a7e5aec7d0a71d9a3a863c0b6/8.8.3/share/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
libexecdir = "/home/martin/LCC/ALP/Interprete-Lambda-Calc-Tipado/.stack-work/install/x86_64-linux-tinfo6/c70a8f8a1954a7b02bc853c8180ce9fb6055b96a7e5aec7d0a71d9a3a863c0b6/8.8.3/libexec/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
sysconfdir = "/home/martin/LCC/ALP/Interprete-Lambda-Calc-Tipado/.stack-work/install/x86_64-linux-tinfo6/c70a8f8a1954a7b02bc853c8180ce9fb6055b96a7e5aec7d0a71d9a3a863c0b6/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
