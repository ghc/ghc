{-# LANGUAGE CPP #-}

module GHC.BaseDir where

import Prelude -- See note [Why do we import Prelude here?]

import Data.List
import System.FilePath

-- Windows
#if defined(mingw32_HOST_OS)
import System.Environment (getExecutablePath)
-- POSIX
#elif defined(darwin_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
import System.Environment (getExecutablePath)
#endif

-- | Expand occurrences of the @$topdir@ interpolation in a string.
expandTopDir :: FilePath -> String -> String
expandTopDir = expandPathVar "topdir"

-- | @expandPathVar var value str@
--
--   replaces occurences of variable @$var@ with @value@ in str.
expandPathVar :: String -> FilePath -> String -> String
expandPathVar var value str
  | Just str' <- stripPrefix ('$':var) str
  , null str' || isPathSeparator (head str')
  = value ++ expandPathVar var value str'
expandPathVar var value (x:xs) = x : expandPathVar var value xs
expandPathVar _ _ [] = []

getBaseDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getBaseDir = Just . (\p -> p </> "lib") . rootDir <$> getExecutablePath
  where
    -- locate the "base dir" when given the path
    -- to the real ghc executable (as opposed to symlink)
    -- that is running this function.
    rootDir :: FilePath -> FilePath
    rootDir = takeDirectory . takeDirectory . normalise
#elif defined(darwin_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
-- on unix, this is a bit more confusing.
-- The layout right now is something like
--
--   /bin/ghc-X.Y.Z <- wrapper script (1)
--   /bin/ghc       <- symlink to wrapper script (2)
--   /lib/ghc-X.Y.Z/bin/ghc <- ghc executable (3)
--   /lib/ghc-X.Y.Z <- $topdir (4)
--
-- As such, we first need to find the absolute location to the
-- binary.
--
-- getExecutablePath will return (3). One takeDirectory will
-- give use /lib/ghc-X.Y.Z/bin, and another will give us (4).
--
-- This of course only works due to the current layout. If
-- the layout is changed, such that we have ghc-X.Y.Z/{bin,lib}
-- this would need to be changed accordingly.
--
getBaseDir = Just . (\p -> p </> "lib") . takeDirectory . takeDirectory <$> getExecutablePath
#else
getBaseDir = return Nothing
#endif
