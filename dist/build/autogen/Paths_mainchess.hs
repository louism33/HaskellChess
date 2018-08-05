module Paths_mainchess (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/louis/Documents/haskell/gloss/chessmasterfolder/mainchess/.cabal-sandbox/bin"
libdir     = "/home/louis/Documents/haskell/gloss/chessmasterfolder/mainchess/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/mainchess-0.1.0.0-GMON1ziQ1i1IdLJemv4kJe"
datadir    = "/home/louis/Documents/haskell/gloss/chessmasterfolder/mainchess/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/mainchess-0.1.0.0"
libexecdir = "/home/louis/Documents/haskell/gloss/chessmasterfolder/mainchess/.cabal-sandbox/libexec"
sysconfdir = "/home/louis/Documents/haskell/gloss/chessmasterfolder/mainchess/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mainchess_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mainchess_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mainchess_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mainchess_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mainchess_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
