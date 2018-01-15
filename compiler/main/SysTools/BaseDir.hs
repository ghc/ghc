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

module SysTools.BaseDir (expandTopDir, findTopDir) where

#include "HsVersions.h"

import GhcPrelude

import Panic

import System.FilePath
import Data.List

-- POSIX
#if defined(darwin_HOST_OS) || defined(linux_HOST_OS)
import System.Environment (getExecutablePath)
#endif

-- Windows
#if defined(mingw32_HOST_OS)
#if MIN_VERSION_Win32(2,5,0)
import qualified System.Win32.Types as Win32
#else
import qualified System.Win32.Info as Win32
#endif
import Exception
import Foreign
import Foreign.C.String
import System.Directory
import System.Win32.Types (DWORD, LPTSTR, HANDLE)
import System.Win32.Types (failIfNull, failIf, iNVALID_HANDLE_VALUE)
import System.Win32.File (createFile,closeHandle, gENERIC_READ, fILE_SHARE_READ, oPEN_EXISTING, fILE_ATTRIBUTE_NORMAL, fILE_FLAG_BACKUP_SEMANTICS )
import System.Win32.DLL (loadLibrary, getProcAddress)
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

-}

-- | Expand occurrences of the @$topdir@ interpolation in a string.
expandTopDir :: FilePath -> String -> String
expandTopDir top_dir str
  | Just str' <- stripPrefix "$topdir" str
  , null str' || isPathSeparator (head str')
  = top_dir ++ expandTopDir top_dir str'
expandTopDir top_dir (x:xs) = x : expandTopDir top_dir xs
expandTopDir _ [] = []

-- | Returns a Unix-format path pointing to TopDir.
findTopDir :: Maybe String -- Maybe TopDir path (without the '-B' prefix).
           -> IO String    -- TopDir (in Unix format '/' separated)
findTopDir (Just minusb) = return (normalise minusb)
findTopDir Nothing
    = do -- Get directory of executable
         maybe_exec_dir <- getBaseDir
         case maybe_exec_dir of
             -- "Just" on Windows, "Nothing" on unix
             Nothing  -> throwGhcExceptionIO (InstallationError "missing -B<dir> option")
             Just dir -> return dir

getBaseDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
-- Assuming we are running ghc, accessed by path  $(stuff)/<foo>/ghc.exe,
-- return the path $(stuff)/lib.
getBaseDir = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> do
                path <- peekCWString buf
                real <- getFinalPath path -- try to resolve symlinks paths
                let libdir = (buildLibDir . sanitize . maybe path id) real
                exists <- doesDirectoryExist libdir
                if exists
                   then return $ Just libdir
                   else fail path
            | otherwise  -> try_size (size * 2)

    -- getFinalPath returns paths in full raw form.
    -- Unfortunately GHC isn't set up to handle these
    -- So if the call succeeded, we need to drop the
    -- \\?\ prefix.
    sanitize s = if "\\\\?\\" `isPrefixOf` s
                    then drop 4 s
                    else s

    buildLibDir :: FilePath -> FilePath
    buildLibDir s =
      (takeDirectory . takeDirectory . normalise $ s) </> "lib"

    fail s = panic ("can't decompose ghc.exe path: " ++ show s)

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32

-- Attempt to resolve symlinks in order to find the actual location GHC
-- is located at. See Trac #11759.
getFinalPath :: FilePath -> IO (Maybe FilePath)
getFinalPath name = do
    dllHwnd <- failIfNull "LoadLibrary"     $ loadLibrary "kernel32.dll"
    -- Note: The API GetFinalPathNameByHandleW is only available starting from Windows Vista.
    -- This means that we can't bind directly to it since it may be missing.
    -- Instead try to find it's address at runtime and if we don't succeed consider the
    -- function failed.
    addr_m  <- (fmap Just $ failIfNull "getProcAddress" $ getProcAddress dllHwnd "GetFinalPathNameByHandleW")
                  `catch` (\(_ :: SomeException) -> return Nothing)
    case addr_m of
      Nothing   -> return Nothing
      Just addr -> do handle  <- failIf (==iNVALID_HANDLE_VALUE) "CreateFile"
                                        $ createFile name
                                                     gENERIC_READ
                                                     fILE_SHARE_READ
                                                     Nothing
                                                     oPEN_EXISTING
                                                     (fILE_ATTRIBUTE_NORMAL .|. fILE_FLAG_BACKUP_SEMANTICS)
                                                     Nothing
                      let fnPtr = makeGetFinalPathNameByHandle $ castPtrToFunPtr addr
                      -- First try to resolve the path to get the actual path
                      -- of any symlinks or other file system redirections that
                      -- may be in place. However this function can fail, and in
                      -- the event it does fail, we need to try using the
                      -- original path and see if we can decompose that.
                      -- If the call fails Win32.try will raise an exception
                      -- that needs to be caught. See #14159
                      path    <- (Win32.try "GetFinalPathName"
                                    (\buf len -> fnPtr handle buf len 0) 512
                                    `finally` closeHandle handle)
                                `catch`
                                 (\(_ :: IOException) -> return name)
                      return $ Just path

type GetFinalPath = HANDLE -> LPTSTR -> DWORD -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "dynamic"
  makeGetFinalPathNameByHandle :: FunPtr GetFinalPath -> GetFinalPath
#elif defined(darwin_HOST_OS) || defined(linux_HOST_OS)
-- on unix, this is a bit more confusing.
-- The layout right now is somehting like
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
