{-# LANGUAGE CPP #-}
-- | Functions for interrogating the current working directory
module Test.Cabal.Workdir where

import Distribution.Simple.Setup
import Distribution.Simple.Configure

import System.Directory
import System.FilePath

#if MIN_VERSION_base(4,6,0)
import System.Environment ( getExecutablePath )
#endif

-- | Guess what the dist directory of a running executable is,
-- by using the conventional layout of built executables
-- in relation to the top of a dist directory.  Will not work
-- if the executable has been installed somewhere else.
guessDistDir :: IO FilePath
guessDistDir = do
#if MIN_VERSION_base(4,6,0)
    exe_path <- canonicalizePath =<< getExecutablePath
    let dist0 = dropFileName exe_path </> ".." </> ".."
    b <- doesFileExist (dist0 </> "setup-config")
#else
    let dist0 = error "no path"
        b = False
#endif
    if b then canonicalizePath dist0
         else findDistPrefOrDefault NoFlag >>= canonicalizePath
