{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Tran (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\PLC1\\.stack-work\\install\\17de8b66\\bin"
libdir     = "C:\\PLC1\\.stack-work\\install\\17de8b66\\lib\\x86_64-windows-ghc-9.2.7\\Tran-0.1.0.0-Ha6meVuZFwW2m58b5C4Tjv-Tran-exe"
dynlibdir  = "C:\\PLC1\\.stack-work\\install\\17de8b66\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\PLC1\\.stack-work\\install\\17de8b66\\share\\x86_64-windows-ghc-9.2.7\\Tran-0.1.0.0"
libexecdir = "C:\\PLC1\\.stack-work\\install\\17de8b66\\libexec\\x86_64-windows-ghc-9.2.7\\Tran-0.1.0.0"
sysconfdir = "C:\\PLC1\\.stack-work\\install\\17de8b66\\etc"

getBinDir     = catchIO (getEnv "Tran_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Tran_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Tran_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Tran_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tran_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tran_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
