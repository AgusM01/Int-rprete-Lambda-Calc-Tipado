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

bindir     = "/home/agusm01/LCC/3erA\241o/ALP/2024/TP2/.stack-work/install/x86_64-linux/2b4830f3a94abc4fa5ada1427aed1851bcb312edccd9be3629997d76abe78856/8.8.3/bin"
libdir     = "/home/agusm01/LCC/3erA\241o/ALP/2024/TP2/.stack-work/install/x86_64-linux/2b4830f3a94abc4fa5ada1427aed1851bcb312edccd9be3629997d76abe78856/8.8.3/lib/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0-2c9qde79sPOLVEqyS9Uqj7-TP2-exe"
dynlibdir  = "/home/agusm01/LCC/3erA\241o/ALP/2024/TP2/.stack-work/install/x86_64-linux/2b4830f3a94abc4fa5ada1427aed1851bcb312edccd9be3629997d76abe78856/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/agusm01/LCC/3erA\241o/ALP/2024/TP2/.stack-work/install/x86_64-linux/2b4830f3a94abc4fa5ada1427aed1851bcb312edccd9be3629997d76abe78856/8.8.3/share/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
libexecdir = "/home/agusm01/LCC/3erA\241o/ALP/2024/TP2/.stack-work/install/x86_64-linux/2b4830f3a94abc4fa5ada1427aed1851bcb312edccd9be3629997d76abe78856/8.8.3/libexec/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
sysconfdir = "/home/agusm01/LCC/3erA\241o/ALP/2024/TP2/.stack-work/install/x86_64-linux/2b4830f3a94abc4fa5ada1427aed1851bcb312edccd9be3629997d76abe78856/8.8.3/etc"

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
