{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_markdown_parser (
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

bindir     = "/home/woodj/.cabal/bin"
libdir     = "/home/woodj/.cabal/lib/x86_64-linux-ghc-8.6.5/markdown-parser-0.1.0.0-8liRj0AZ3q99URn0tMZcaz"
dynlibdir  = "/home/woodj/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/woodj/.cabal/share/x86_64-linux-ghc-8.6.5/markdown-parser-0.1.0.0"
libexecdir = "/home/woodj/.cabal/libexec/x86_64-linux-ghc-8.6.5/markdown-parser-0.1.0.0"
sysconfdir = "/home/woodj/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "markdown_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "markdown_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "markdown_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "markdown_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "markdown_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "markdown_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
