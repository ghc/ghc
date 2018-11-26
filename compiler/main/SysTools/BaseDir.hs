{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2017
--
-- Finding the compiler's base directory.
--
-----------------------------------------------------------------------------
-}

module SysTools.BaseDir
  ( expandTopDir, expandToolDir
  , findTopDir, findToolDir
  ) where

#include "HsVersions.h"

import GhcPrelude

import Panic

import System.Environment (lookupEnv)
import System.FilePath
import Data.List

-- POSIX
#if defined(darwin_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
import System.Environment (getExecutablePath)
#endif

-- Windows
#if defined(mingw32_HOST_OS)
import System.Environment (getExecutablePath)
import System.Directory (doesDirectoryExist)
#endif

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

{-
Note [topdir: How GHC finds its files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC needs various support files (library packages, RTS etc), plus
various auxiliary programs (cp, gcc, etc).  It starts by finding topdir,
the root of GHC's support files

On Unix:
  - ghc always has a shell wrapper that passes a -B<dir> option

On Windows:
  - ghc never has a shell wrapper.
  - we can find the location of the ghc binary, which is
        $topdir/<foo>/<something>.exe
    where <something> may be "ghc", "ghc-stage2", or similar
  - we strip off the "<foo>/<something>.exe" to leave $topdir.

from topdir we can find package.conf, ghc-asm, etc.


Note [tooldir: How GHC finds mingw and perl on Windows]

GHC has some custom logic on Windows for finding the mingw
toolchain and perl. Depending on whether GHC is built
with the make build system or Hadrian, and on whether we're
running a bindist, we might find the mingw toolchain and perl
either under $topdir/../{mingw, perl}/ or
$topdir/../../{mingw, perl}/.

-}

-- | Expand occurrences of the @$topdir@ interpolation in a string.
expandTopDir :: FilePath -> String -> String
expandTopDir = expandPathVar "topdir"

-- | Expand occurrences of the @$tooldir@ interpolation in a string
-- on Windows, leave the string untouched otherwise.
expandToolDir :: Maybe FilePath -> String -> String
#if defined(mingw32_HOST_OS)
expandToolDir (Just tool_dir) s = expandPathVar "tooldir" tool_dir s
expandToolDir Nothing         _ = panic "Could not determine $tooldir"
#else
expandToolDir _ s = s
#endif

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

-- | Returns a Unix-format path pointing to TopDir.
findTopDir :: Maybe String -- Maybe TopDir path (without the '-B' prefix).
           -> IO String    -- TopDir (in Unix format '/' separated)
findTopDir (Just minusb) = return (normalise minusb)
findTopDir Nothing
    = do -- The _GHC_TOP_DIR environment variable can be used to specify
         -- the top dir when the -B argument is not specified. It is not
         -- intended for use by users, it was added specifically for the
         -- purpose of running GHC within GHCi.
         maybe_env_top_dir <- lookupEnv "_GHC_TOP_DIR"
         case maybe_env_top_dir of
             Just env_top_dir -> return env_top_dir
             Nothing -> do
                 -- Get directory of executable
                 maybe_exec_dir <- getBaseDir
                 case maybe_exec_dir of
                     -- "Just" on Windows, "Nothing" on unix
                     Nothing -> throwGhcExceptionIO $
                         InstallationError "missing -B<dir> option"
                     Just dir -> return dir

getBaseDir :: IO (Maybe String)

#if defined(mingw32_HOST_OS)

-- locate the "base dir" when given the path
-- to the real ghc executable (as opposed to symlink)
-- that is running this function.
rootDir :: FilePath -> FilePath
rootDir = takeDirectory . takeDirectory . normalise

getBaseDir = Just . (\p -> p </> "lib") . rootDir <$> getExecutablePath
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

-- See Note [tooldir: How GHC finds mingw and perl on Windows]
-- Returns @Nothing@ when not on Windows.
-- When called on Windows, it either throws an error when the
-- tooldir can't be located, or returns @Just tooldirpath@.
findToolDir
  :: FilePath -- ^ topdir
  -> IO (Maybe FilePath)
#if defined(mingw32_HOST_OS)
findToolDir top_dir = go 0 (top_dir </> "..")
  where maxDepth = 3
        go :: Int -> FilePath -> IO (Maybe FilePath)
        go k path
          | k == maxDepth = throwGhcExceptionIO $
              InstallationError "could not detect mingw toolchain"
          | otherwise = do
              oneLevel <- doesDirectoryExist (path </> "mingw")
              if oneLevel
                then return (Just path)
                else go (k+1) (path </> "..")
#else
findToolDir _ = return Nothing
#endif
