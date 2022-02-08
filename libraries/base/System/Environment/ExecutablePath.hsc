{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.ExecutablePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function to retrieve the absolute filepath of the current executable.
--
-- @since 4.6.0.0
-----------------------------------------------------------------------------

module System.Environment.ExecutablePath
  ( getExecutablePath
  , executablePath
  ) where

-- The imports are purposely kept completely disjoint to prevent edits
-- to one OS implementation from breaking another.

#if defined(darwin_HOST_OS)
import Control.Exception (catch, throw)
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Error (isDoesNotExistError)
import System.Posix.Internals
#elif defined(linux_HOST_OS)
import Data.List (isSuffixOf)
import Foreign.C
import Foreign.Marshal.Array
import System.Posix.Internals
#elif defined(freebsd_HOST_OS) || defined(netbsd_HOST_OS)
import Control.Exception (catch, throw)
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Error (isDoesNotExistError)
import System.Posix.Internals
#include <sys/types.h>
#include <sys/sysctl.h>
#elif defined(mingw32_HOST_OS)
import Control.Exception
import Data.List (isPrefixOf)
import Data.Word
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Windows
#include <windows.h>
#include <stdint.h>
#else
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Internals
#endif

-- The exported function is defined outside any if-guard to make sure
-- every OS implements it with the same type.

-- | Returns the absolute pathname of the current executable,
-- or @argv[0]@ if the operating system does not provide a reliable
-- way query the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
--
-- Since base 4.11.0.0, 'getExecutablePath' resolves symlinks on Windows.
-- If an executable is launched through a symlink, 'getExecutablePath'
-- returns the absolute path of the original executable.
--
-- If the executable has been deleted, behaviour is ill-defined and
-- varies by operating system.  See 'executablePath' for a more
-- reliable way to query the current executable.
--
-- @since 4.6.0.0
getExecutablePath :: IO FilePath

-- | Get an action to query the absolute pathname of the current executable.
--
-- If the operating system provides a reliable way to determine the current
-- executable, return the query action, otherwise return @Nothing@.  The action
-- is defined on FreeBSD, Linux, MacOS, NetBSD, and Windows.
--
-- Even where the query action is defined, there may be situations where no
-- result is available, e.g. if the executable file was deleted while the
-- program is running.  Therefore the result of the query action is a @Maybe
-- FilePath@.
--
-- Note that for scripts and interactive sessions, the result is the path to
-- the interpreter (e.g. ghci.)
--
-- @since 4.17.0.0
executablePath :: Maybe (IO (Maybe FilePath))


--------------------------------------------------------------------------------
-- Mac OS X

#if defined(darwin_HOST_OS)

type UInt32 = Word32

foreign import ccall unsafe "mach-o/dyld.h _NSGetExecutablePath"
    c__NSGetExecutablePath :: CString -> Ptr UInt32 -> IO CInt

-- | Returns the path of the main executable. The path may be a
-- symbolic link and not the real file.
--
-- See dyld(3)
_NSGetExecutablePath :: IO FilePath
_NSGetExecutablePath =
    allocaBytes 1024 $ \ buf ->  -- PATH_MAX is 1024 on OS X
    alloca $ \ bufsize -> do
        poke bufsize 1024
        status <- c__NSGetExecutablePath buf bufsize
        if status == 0
            then peekFilePath buf
            else do reqBufsize <- fromIntegral `fmap` peek bufsize
                    allocaBytes reqBufsize $ \ newBuf -> do
                        status2 <- c__NSGetExecutablePath newBuf bufsize
                        if status2 == 0
                             then peekFilePath newBuf
                             else errorWithoutStackTrace "_NSGetExecutablePath: buffer too small"

foreign import ccall unsafe "stdlib.h realpath"
    c_realpath :: CString -> CString -> IO CString

-- | Resolves all symbolic links, extra \/ characters, and references
-- to \/.\/ and \/..\/. Returns an absolute pathname.
--
-- See realpath(3)
realpath :: FilePath -> IO FilePath
realpath path =
    withFilePath path $ \ fileName ->
    allocaBytes 1024 $ \ resolvedName -> do
        _ <- throwErrnoIfNull "realpath" $ c_realpath fileName resolvedName
        peekFilePath resolvedName

getExecutablePath = _NSGetExecutablePath >>= realpath

-- realpath(3) fails with ENOENT file does not exist (e.g. was deleted)
executablePath = Just (fmap Just getExecutablePath `catch` f)
  where
  f e | isDoesNotExistError e = pure Nothing
      | otherwise             = throw e

--------------------------------------------------------------------------------
-- Linux

#elif defined(linux_HOST_OS)

foreign import ccall unsafe "readlink"
    c_readlink :: CString -> CString -> CSize -> IO CInt

-- | Reads the @FilePath@ pointed to by the symbolic link and returns
-- it.
--
-- See readlink(2)
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
    allocaArray0 4096 $ \buf ->
        withFilePath file $ \s -> do
            len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $
                   c_readlink s buf 4096
            peekFilePathLen (buf,fromIntegral len)

getExecutablePath = readSymbolicLink $ "/proc/self/exe"

executablePath = Just (check <$> getExecutablePath) where
  -- procfs(5): If the pathname has been unlinked, the symbolic link will
  -- contain the string '(deleted)' appended to the original pathname.
  --
  -- See also https://gitlab.haskell.org/ghc/ghc/-/issues/10957
  check s | "(deleted)" `isSuffixOf` s = Nothing
          | otherwise = Just s

--------------------------------------------------------------------------------
-- FreeBSD / NetBSD

#elif defined(freebsd_HOST_OS) || defined(netbsd_HOST_OS)

foreign import ccall unsafe "sysctl"
  c_sysctl
    :: Ptr CInt   -- MIB
    -> CUInt      -- MIB size
    -> Ptr CChar  -- old / current value buffer
    -> Ptr CSize  -- old / current value buffer size
    -> Ptr CChar  -- new value
    -> CSize      -- new value size
    -> IO CInt    -- result

getExecutablePath = do
  withArrayLen mib $ \n mibPtr -> do
    let mibLen = fromIntegral n
    alloca $ \bufSizePtr -> do
      status <- c_sysctl mibPtr mibLen nullPtr bufSizePtr nullPtr 0
      case status of
        0 -> do
          reqBufSize <- fromIntegral <$> peek bufSizePtr
          allocaBytes reqBufSize $ \buf -> do
            newStatus <- c_sysctl mibPtr mibLen buf bufSizePtr nullPtr 0
            case newStatus of
              0 -> peekFilePath buf
              _ -> barf
        _ -> barf
  where
    barf = throwErrno "getExecutablePath"
    mib =
#  if defined(freebsd_HOST_OS)
      [ (#const CTL_KERN)
      , (#const KERN_PROC)
      , (#const KERN_PROC_PATHNAME)
      , -1   -- current process
      ]
#  elif defined(netbsd_HOST_OS)
      [ (#const CTL_KERN)
      , (#const KERN_PROC_ARGS)
      , -1   -- current process
      , (#const KERN_PROC_PATHNAME)
      ]
#  endif

executablePath = Just (fmap Just getExecutablePath `catch` f)
  where
  -- The sysctl fails with errno ENOENT when executable has been deleted;
  -- see https://gitlab.haskell.org/ghc/ghc/-/issues/12377#note_321346.
  f e | isDoesNotExistError e = pure Nothing

  -- As far as I know, ENOENT is the only kind of failure that should be
  -- expected and handled.  Re-throw other errors.
      | otherwise             = throw e


--------------------------------------------------------------------------------
-- Windows

#elif defined(mingw32_HOST_OS)

# if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif

getExecutablePath = go 2048  -- plenty, PATH_MAX is 512 under Win32
  where
    go size = allocaArray (fromIntegral size) $ \ buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
            0 -> errorWithoutStackTrace "getExecutablePath: GetModuleFileNameW returned an error"
            _ | ret < size -> do
                  path <- peekCWString buf
                  real <- getFinalPath path
                  exists <- withCWString real c_pathFileExists
                  if exists
                    then return real
                    else fail path
              | otherwise  -> go (size * 2)

-- Windows prevents deletion of executable file while program is running.
-- Therefore return @Just@ of the result.
executablePath = Just (Just <$> getExecutablePath)

-- | Returns the final path of the given path. If the given
--   path is a symbolic link, the returned value is the
--   path the (possibly chain of) symbolic link(s) points to.
--   Otherwise, the original path is returned, even when the filepath
--   is incorrect.
--
-- Adapted from:
-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa364962.aspx
getFinalPath :: FilePath -> IO FilePath
getFinalPath path = withCWString path $ \s ->
  bracket (createFile s) c_closeHandle $ \h -> do
    let invalid = h == iNVALID_HANDLE_VALUE
    if invalid then pure path else go h bufSize

  where go h sz = allocaArray (fromIntegral sz) $ \outPath -> do
          ret <- c_getFinalPathHandle h outPath sz (#const FILE_NAME_OPENED)
          if ret < sz
            then sanitize . rejectUNCPath <$> peekCWString outPath
            else go h (2 * sz)

        sanitize s
          | "\\\\?\\" `isPrefixOf` s = drop 4 s
          | otherwise                = s

        -- see https://gitlab.haskell.org/ghc/ghc/issues/14460
        rejectUNCPath s
          | "\\\\?\\UNC\\" `isPrefixOf` s = path
          | otherwise                     = s

        -- the initial size of the buffer in which we store the
        -- final path; if this is not enough, we try with a buffer of
        -- size 2^k * bufSize, for k = 1, 2, 3, ... until the buffer
        -- is large enough.
        bufSize = 1024

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
    c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32

foreign import WINDOWS_CCONV unsafe "windows.h PathFileExistsW"
    c_pathFileExists :: CWString -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h CreateFileW"
    c_createFile :: CWString
                 -> Word32
                 -> Word32
                 -> Ptr ()
                 -> Word32
                 -> Word32
                 -> Ptr ()
                 -> IO (Ptr ())

createFile :: CWString -> IO (Ptr ())
createFile file =
  c_createFile file (#const GENERIC_READ)
                    (#const FILE_SHARE_READ)
                    nullPtr
                    (#const OPEN_EXISTING)
                    (#const FILE_ATTRIBUTE_NORMAL)
                    nullPtr

foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
  c_closeHandle  :: Ptr () -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetFinalPathNameByHandleW"
  c_getFinalPathHandle :: Ptr () -> CWString -> Word32 -> Word32 -> IO Word32

--------------------------------------------------------------------------------
-- Fallback to argv[0]

#else

foreign import ccall unsafe "getFullProgArgv"
    c_getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getExecutablePath =
    alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        c_getFullProgArgv p_argc p_argv
        argc <- peek p_argc
        if argc > 0
            -- If argc > 0 then argv[0] is guaranteed by the standard
            -- to be a pointer to a null-terminated string.
            then peek p_argv >>= peek >>= peekFilePath
            else errorWithoutStackTrace $ "getExecutablePath: " ++ msg
  where msg = "no OS specific implementation and program name couldn't be " ++
              "found in argv"

executablePath = Nothing

--------------------------------------------------------------------------------

#endif
