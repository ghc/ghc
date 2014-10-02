{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Windows
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Windows functionality used by several modules.
--
-- ToDo: this just duplicates part of System.Win32.Types, which isn't
-- available yet.  We should move some Win32 functionality down here,
-- maybe as part of the grand reorganisation of the base package...
--
-----------------------------------------------------------------------------

module GHC.Windows (
        -- * Types
        BOOL,
        LPBOOL,
        BYTE,
        DWORD,
        UINT,
        ErrCode,
        HANDLE,
        LPWSTR,
        LPTSTR,

        -- * Constants
        iNFINITE,
        iNVALID_HANDLE_VALUE,

        -- * System errors
        throwGetLastError,
        failWith,
        getLastError,
        getErrorMessage,
        errCodeToIOError,

        -- ** Guards for system calls that might fail
        failIf,
        failIf_,
        failIfNull,
        failIfZero,
        failIfFalse_,
        failUnlessSuccess,
        failUnlessSuccessOr,

        -- ** Mapping system errors to errno
        -- $errno
        c_maperrno,
        c_maperrno_func,
    ) where

import Data.Char
import Data.OldList
import Data.Maybe
import Data.Word
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import GHC.Base
import GHC.IO
import GHC.Num
import System.IO.Error

import qualified Numeric

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

type BOOL    = Bool
type LPBOOL  = Ptr BOOL
type BYTE    = Word8
type DWORD   = Word32
type UINT    = Word32
type ErrCode = DWORD
type HANDLE  = Ptr ()
type LPWSTR  = Ptr CWchar

-- | Be careful with this.  LPTSTR can mean either WCHAR* or CHAR*, depending
-- on whether the UNICODE macro is defined in the corresponding C code.
-- Consider using LPWSTR instead.
type LPTSTR = LPWSTR

iNFINITE :: DWORD
iNFINITE = 0xFFFFFFFF -- urgh

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = wordPtrToPtr (-1)

-- | Get the last system error, and throw it as an 'IOError' exception.
throwGetLastError :: String -> IO a
throwGetLastError where_from =
    getLastError >>= failWith where_from

-- | Convert a Windows error code to an exception, then throw it.
failWith :: String -> ErrCode -> IO a
failWith fn_name err_code =
    errCodeToIOError fn_name err_code >>= throwIO

-- | Convert a Windows error code to an exception.
errCodeToIOError :: String -> ErrCode -> IO IOError
errCodeToIOError fn_name err_code = do
    msg <- getErrorMessage err_code

    -- turn GetLastError() into errno, which errnoToIOError knows
    -- how to convert to an IOException we can throw.
    -- XXX we should really do this directly.
    let errno = c_maperrno_func err_code

    let msg' = dropWhileEnd isSpace msg -- drop trailing \n
        ioerror = errnoToIOError fn_name errno Nothing Nothing
                    `ioeSetErrorString` msg'
    return ioerror

-- | Get a string describing a Windows error code.  This uses the
-- @FormatMessage@ system call.
getErrorMessage :: ErrCode -> IO String
getErrorMessage err_code =
    mask_ $ do
        c_msg <- c_getErrorMessage err_code
        if c_msg == nullPtr
          then return $ "Error 0x" ++ Numeric.showHex err_code ""
          else do msg <- peekCWString c_msg
                  -- We ignore failure of freeing c_msg, given we're already failing
                  _ <- localFree c_msg
                  return msg

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
    v <- act
    if p v then throwGetLastError wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
    v <- act
    if p v then throwGetLastError wh else return ()

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIfZero :: (Eq a, Num a) => String -> IO a -> IO a
failIfZero = failIf (== 0)

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

failUnlessSuccess :: String -> IO ErrCode -> IO ()
failUnlessSuccess fn_name act = do
    r <- act
    if r == 0 then return () else failWith fn_name r

failUnlessSuccessOr :: ErrCode -> String -> IO ErrCode -> IO Bool
failUnlessSuccessOr val fn_name act = do
    r <- act
    if r == 0 then return False
        else if r == val then return True
        else failWith fn_name r

-- $errno
--
-- On Windows, @errno@ is defined by msvcrt.dll for compatibility with other
-- systems, and is distinct from the system error as returned
-- by @GetLastError@.

-- | Map the last system error to an errno value, and assign it to @errno@.
foreign import ccall unsafe "maperrno"             -- in Win32Utils.c
   c_maperrno :: IO ()

-- | Pure function variant of 'c_maperrno' that does not call @GetLastError@
-- or modify @errno@.
foreign import ccall unsafe "maperrno_func"        -- in Win32Utils.c
   c_maperrno_func :: ErrCode -> Errno

foreign import ccall unsafe "base_getErrorMessage" -- in Win32Utils.c
    c_getErrorMessage :: DWORD -> IO LPWSTR

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    localFree :: Ptr a -> IO (Ptr a)

-- | Get the last system error produced in the current thread.
foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
    getLastError :: IO ErrCode
