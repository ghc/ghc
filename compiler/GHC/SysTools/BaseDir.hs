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

import GHC.Prelude

-- See Note [Base Dir] for why some of this logic is shared with ghc-pkg.
import GHC.BaseDir

import GHC.Utils.Panic

import System.Environment (lookupEnv)
import System.FilePath

-- Windows
#if defined(mingw32_HOST_OS)
import System.Directory (doesDirectoryExist)
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has some custom logic on Windows for finding the mingw
toolchain and perl. Depending on whether GHC is built
with the make build system or Hadrian, and on whether we're
running a bindist, we might find the mingw toolchain
either under $topdir/../{mingw, perl}/ or
$topdir/../../{mingw, perl}/.

This story is long and with lots of twist and turns..  But lets talk about how
the build system finds and wires through the toolchain information.

1) It all starts in configure.ac which has two modes it operates on:
   a) The default is where `EnableDistroToolchain` is false.  This indicates
      that we want to use the in-tree bundled toolchains.  In this mode we will
      download and unpack some custom toolchains into the `inplace/mingw` folder
      and everything is pointed to that folder.
   b) The second path is when `EnableDistroToolchain` is true.  This makes the
      toolchain behave a lot like Linux, in that  the environment is queried for
      information on the tools we require.

  From configure.ac we export the standard variables to set the paths to the
  tools for the build system to use.

2) After we have the path to the tools we have to generate the right paths to
   store in the settings file for ghc to use.  This is done in aclocal.m4.
   Again we have two modes of operation:
   a) If not `EnableDistroToolchain` the paths are rewritten to paths using a
      variable `$tooldir` as we need an absolute path.  $tooldir is filled in by
      the `expandToolDir` function in this module at GHC startup.
   b) When `EnableDistroToolchain` then instead of filling in a absolute path
      we fill in just the program name.  The assumption here is that at runtime
      the environment GHC is operating on will be the same as the one configure
      was run in.  This means we expect `gcc, ld, as` etc to be on the PATH.

  From `aclocal.m4` we export a couple of variables starting with `Settings`
  which will be used to generate the settings file.

3) The next step is to generate the settings file, this is where things diverge
   based on the build system.  Both Make and Hadrian handle this differently:

make)
  Make deals with this rather simply.  As an output of configure.ac
  `config.mk.in` is processed and `config.mk` generated which has the values we
  set in `aclocal.m4`. This allows the rest of the build system to have access
  to these and other values determined by configure.

  Based on this file, `rts/include/ghc.mk` when ran will produce the settings file
  by echoing the values into a the final file.  Coincidentally this is also
  where `ghcplatform.h` and `ghcversion.h` generated which contains information
  about the build platform and sets CPP for use by the entire build.

hadrian)
  For hadrian the file `cfg/system.config.in` is preprocessed by configure and
  the output written to `system.config`.  This serves the same purpose as
  `config.mk` but it rewrites the values that were exported.  As an example
  `SettingsCCompilerCommand` is rewritten to `settings-c-compiler-command`.

  Next up is `src/Oracles/Settings.hs` which makes from some Haskell ADT to
  the settings `keys` in the `system.config`.  As an example,
  `settings-c-compiler-command` is mapped to
  `SettingsFileSetting_CCompilerCommand`.

  The last part of this is the `generateSettings` in `src/Rules/Generate.hs`
  which produces the desired settings file out of Hadrian. This is the
  equivalent to `rts/include/ghc.mk`.

--

So why do we have these? On Windows there's no such thing as a platform compiler
and as such we need to provide GCC and binutils.  The easiest way is to bundle
these with the compiler and wire them up.  This gives you a relocatable
binball.  This works fine for most users.  However mingw-w64 have a different
requirement.  They require all packages in the repo to be compiled using the
same version of the compiler.  So it means when they are rebuilding the world to
add support for GCC X, they expect all packages to have been compiled with GCC X
which is a problem since we ship an older GCC version.

GHC is a package in mingw-w64 because there are Haskell packages in the
repository which of course requires a Haskell compiler.  To help them we
provide the override which allows GHC to instead of using an inplace compiler to
play nice with the system compiler instead.
-}

-- | Expand occurrences of the @$tooldir@ interpolation in a string
-- on Windows, leave the string untouched otherwise.
expandToolDir
  :: Bool -- ^ whether we are use the ambiant mingw toolchain
  -> Maybe FilePath -- ^ tooldir
  -> String -> String
#if defined(mingw32_HOST_OS)
expandToolDir False (Just tool_dir) s = expandPathVar "tooldir" tool_dir s
expandToolDir False Nothing         _ = panic "Could not determine $tooldir"
expandToolDir True  _               s = s
#else
expandToolDir _ _ s = s
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


-- See Note [tooldir: How GHC finds mingw on Windows]
-- Returns @Nothing@ when not on Windows.
-- When called on Windows, it either throws an error when the
-- tooldir can't be located, or returns @Just tooldirpath@.
-- If the distro toolchain is being used we treat Windows the same as Linux
findToolDir
  :: Bool -- ^ whether we are use the ambiant mingw toolchain
  -> FilePath -- ^ topdir
  -> IO (Maybe FilePath)
#if defined(mingw32_HOST_OS)
findToolDir False top_dir = go 0 (top_dir </> "..") []
  where maxDepth = 3
        go :: Int -> FilePath -> [FilePath] -> IO (Maybe FilePath)
        go k path tried
          | k == maxDepth = throwGhcExceptionIO $
              InstallationError $ "could not detect mingw toolchain in the following paths: " ++ show tried
          | otherwise = do
              let try = path </> "mingw"
              let tried' = tried ++ [try]
              oneLevel <- doesDirectoryExist try
              if oneLevel
                then return (Just path)
                else go (k+1) (path </> "..") tried'
findToolDir True _ = return Nothing
#else
findToolDir _ _ = return Nothing
#endif
