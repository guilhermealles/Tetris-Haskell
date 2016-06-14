module Paths_Tetris_Haskell (
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

bindir     = "/Users/Alles/Workspace/Tetris-Haskell/.cabal-sandbox/bin"
libdir     = "/Users/Alles/Workspace/Tetris-Haskell/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.3/Tetris-Haskell-0.1.0.0-KVO1s2vVxiD6v6GeGIamtr"
datadir    = "/Users/Alles/Workspace/Tetris-Haskell/.cabal-sandbox/share/x86_64-osx-ghc-7.10.3/Tetris-Haskell-0.1.0.0"
libexecdir = "/Users/Alles/Workspace/Tetris-Haskell/.cabal-sandbox/libexec"
sysconfdir = "/Users/Alles/Workspace/Tetris-Haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tetris_Haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tetris_Haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Tetris_Haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tetris_Haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tetris_Haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
