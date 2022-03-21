
{-# LANGUAGE ScopedTypeVariables #-}

{-
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2003
--
-- Access to system tools: gcc, cp, rm etc
--
-----------------------------------------------------------------------------
-}

module GHC.SysTools (
        -- * Initialisation
        initSysTools,

        -- * Interface to system tools
        module GHC.SysTools.Tasks,
        module GHC.SysTools.Info,

        -- * Fast file copy
        copyFile,
        copyHandle,
        copyWithHeader,

        -- * General utilities
        Option(..),
        expandTopDir,
 ) where

import GHC.Prelude


import GHC.Utils.Panic
import GHC.Driver.Session

import GHC.Linker.ExtraObj
import GHC.SysTools.Info
import GHC.SysTools.Tasks
import GHC.SysTools.BaseDir
import GHC.Settings.IO

import Control.Monad.Trans.Except (runExceptT)
import System.IO
import Foreign.Marshal.Alloc (allocaBytes)
import System.Directory (copyFile)

{-
Note [How GHC finds toolchain utilities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC.SysTools.initSysProgs figures out exactly where all the auxiliary programs
are, and initialises mutable variables to make it easy to call them.
To do this, it makes use of definitions in Config.hs, which is a Haskell
file containing variables whose value is figured out by the build system.

Config.hs contains two sorts of things

  cGCC,         The *names* of the programs
  cCPP            e.g.  cGCC = gcc
  cUNLIT                cCPP = gcc -E
  etc           They do *not* include paths


  cUNLIT_DIR   The *path* to the directory containing unlit, split etc
  cSPLIT_DIR   *relative* to the root of the build tree,
                   for use when running *in-place* in a build tree (only)


---------------------------------------------
NOTES for an ALTERNATIVE scheme (i.e *not* what is currently implemented):

Another hair-brained scheme for simplifying the current tool location
nightmare in GHC: Simon originally suggested using another
configuration file along the lines of GCC's specs file - which is fine
except that it means adding code to read yet another configuration
file.  What I didn't notice is that the current package.conf is
general enough to do this:

Package
    {name = "tools",    import_dirs = [],  source_dirs = [],
     library_dirs = [], hs_libraries = [], extra_libraries = [],
     include_dirs = [], c_includes = [],   package_deps = [],
     extra_ghc_opts = ["-pgmc/usr/bin/gcc","-pgml${topdir}/bin/unlit", ... etc.],
     extra_cc_opts = [], extra_ld_opts = []}

Which would have the advantage that we get to collect together in one
place the path-specific package stuff with the path-specific tool
stuff.
                End of NOTES
---------------------------------------------

************************************************************************
*                                                                      *
\subsection{Initialisation}
*                                                                      *
************************************************************************
-}


initSysTools :: String          -- TopDir path
             -> IO Settings     -- Set all the mutable variables above, holding
                                --      (a) the system programs
                                --      (b) the package-config file
                                --      (c) the GHC usage message
initSysTools top_dir = do
  res <- runExceptT $ initSettings top_dir
  case res of
    Right a -> pure a
    Left (SettingsError_MissingData msg) -> pgmError msg
    Left (SettingsError_BadData msg) -> pgmError msg

{- Note [Windows stack allocations]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See: #8870 (and #8834 for related info) and #12186

On Windows, occasionally we need to grow the stack. In order to do
this, we would normally just bump the stack pointer - but there's a
catch on Windows.

If the stack pointer is bumped by more than a single page, then the
pages between the initial pointer and the resulting location must be
properly committed by the Windows virtual memory subsystem. This is
only needed in the event we bump by more than one page (i.e 4097 bytes
or more).

Windows compilers solve this by emitting a call to a special function
called _chkstk, which does this committing of the pages for you.

The reason this was causing a segfault was because due to the fact the
new code generator tends to generate larger functions, we needed more
stack space in GHC itself. In the x86 codegen, we needed approximately
~12kb of stack space in one go, which caused the process to segfault,
as the intervening pages were not committed.

GCC can emit such a check for us automatically but only when the flag
-fstack-check is used.

See https://gcc.gnu.org/onlinedocs/gnat_ugn/Stack-Overflow-Checking.html
for more information.

-}

-- | Copy remaining bytes from the first Handle to the second one
copyHandle :: Handle -> Handle -> IO ()
copyHandle hin hout = do
  let buf_size = 8192
  allocaBytes buf_size $ \ptr -> do
    let go = do
          c <- hGetBuf hin ptr buf_size
          hPutBuf hout ptr c
          if c == 0 then return () else go
    go

-- | Copy file after printing the given header
copyWithHeader :: String -> FilePath -> FilePath -> IO ()
copyWithHeader header from to =
  withBinaryFile to WriteMode $ \hout -> do
    -- write the header string in UTF-8.  The header is something like
    --   {-# LINE "foo.hs" #-}
    -- and we want to make sure a Unicode filename isn't mangled.
    hSetEncoding hout utf8
    hPutStr hout header
    withBinaryFile from ReadMode $ \hin ->
      copyHandle hin hout
