{-# LANGUAGE CPP #-}

-- | Note [Base Dir]
-- ~~~~~~~~~~~~~~~~~
--
-- GHC's base directory or top directory containers miscellaneous settings and
-- the package database.  The main compiler of course needs this directory to
-- read those settings and read and write packages. ghc-pkg uses it to find the
-- global package database too.
--
-- In the interest of making GHC builds more relocatable, many settings also
-- will expand `${top_dir}` inside strings so GHC doesn't need to know it's on
-- installation location at build time. ghc-pkg also can expand those variables
-- and so needs the top dir location to do that too.
module GHC.BaseDir where

import Prelude -- See note [Why do we import Prelude here?]

import System.FilePath

-- Windows and POSIX
#if defined(mingw32_HOST_OS) || \
    defined(darwin_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
import System.Environment (getExecutablePath)
#endif

-- | Calculate the location of the base dir
getBaseDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS) \
    defined(darwin_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
-- Note [How GHC finds topdir relocatably].
--
-- On Windows, this is straight forward:
--
--   $topdir/<foo>/<something>.exe (3) <- ghc executable (3)
--   $topdir/ <- $topdir (4)
--
-- On Unix, this is a bit more confusing. The layout right now is
-- something like:
--
--   /bin/<something>-X.Y.Z <- wrapper script (1)
--   /bin/<something> <- symlink to wrapper script (2)
--   /lib/ghc-X.Y.Z/bin/<something> <- ghc or other executable (3)
--   /lib/ghc-X.Y.Z <- $topdir (4)
--
-- As such, we first need to find the absolute location to the binary,
-- then go up two directories.
--
-- `getExecutablePath` will return the real exacutable. One
-- `takeDirectory` will give use /lib/ghc-X.Y.Z/bin or its Windows
-- equivalent, and another will give us (4).
--
-- This of course only works due to the current layout. If the layout is
-- changed, such that we have ghc-X.Y.Z/{bin,lib} this would need to be
-- changed accordingly.
getBaseDir = Just . (\p -> p </> "lib") . rootDir <$> getExecutablePath
  where
    -- Locate the "base dir" when given the path to the real ghc
    -- executable (as opposed to symlink) that is running this function.
    -- `normalise` is harmless in the Windows no-symlink case so no need
    -- to special-case.
    rootDir :: FilePath -> FilePath
    rootDir = takeDirectory . takeDirectory . normalise
#else
getBaseDir = return Nothing
#endif
