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

module GHC.SysTools.BaseDir
  ( expandTopDir, expandToolDir
  , findTopDir, findToolDir
  , tryFindTopDir
  ) where

#include "HsVersions.h"

import GHC.Prelude

-- See note [Base Dir] for why some of this logic is shared with ghc-pkg.
import GHC.BaseDir

import GHC.Utils.Panic

import System.Environment (lookupEnv)
import System.FilePath

-- Windows
#if defined(mingw32_HOST_OS)
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


Note [tooldir: How GHC finds mingw on Windows]

GHC has some custom logic on Windows for finding the mingw
toolchain and perl. Depending on whether GHC is built
with the make build system or Hadrian, and on whether we're
running a bindist, we might find the mingw toolchain and perl
either under $topdir/../{mingw, perl}/ or
$topdir/../../{mingw, perl}/.

-}

-- | Expand occurrences of the @$tooldir@ interpolation in a string
-- on Windows, leave the string untouched otherwise.
expandToolDir :: Maybe FilePath -> String -> String
#if defined(mingw32_HOST_OS)
expandToolDir (Just tool_dir) s = expandPathVar "tooldir" tool_dir s
expandToolDir Nothing         _ = panic "Could not determine $tooldir"
#else
expandToolDir _ s = s
#endif

-- | Returns a Unix-format path pointing to TopDir.
findTopDir :: Maybe String -- Maybe TopDir path (without the '-B' prefix).
           -> IO String    -- TopDir (in Unix format '/' separated)
findTopDir m_minusb = do
  maybe_exec_dir <- tryFindTopDir m_minusb
  case maybe_exec_dir of
      -- "Just" on Windows, "Nothing" on unix
      Nothing -> throwGhcExceptionIO $
          InstallationError "missing -B<dir> option"
      Just dir -> return dir

tryFindTopDir
  :: Maybe String -- ^ Maybe TopDir path (without the '-B' prefix).
  -> IO (Maybe String) -- ^ TopDir (in Unix format '/' separated)
tryFindTopDir (Just minusb) = return $ Just $ normalise minusb
tryFindTopDir Nothing
    = do -- The _GHC_TOP_DIR environment variable can be used to specify
         -- the top dir when the -B argument is not specified. It is not
         -- intended for use by users, it was added specifically for the
         -- purpose of running GHC within GHCi.
         maybe_env_top_dir <- lookupEnv "_GHC_TOP_DIR"
         case maybe_env_top_dir of
             Just env_top_dir -> return $ Just env_top_dir
             -- Try directory of executable
             Nothing -> getBaseDir


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
