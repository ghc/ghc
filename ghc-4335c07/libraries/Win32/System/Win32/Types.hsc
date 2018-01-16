#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Types
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Types
        ( module System.Win32.Types
        , nullPtr
        ) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (bracket, throwIO)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Char (isSpace)
import Data.Int (Int32, Int64, Int16)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Error (Errno(..), errnoToIOError)
import Foreign.C.String (newCWString, withCWStringLen)
import Foreign.C.String (peekCWString, peekCWStringLen, withCWString)
import Foreign.C.Types (CChar, CUChar, CWchar, CInt(..), CIntPtr(..), CUIntPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_)
import Foreign.Ptr (FunPtr, Ptr, nullPtr, ptrToIntPtr)
import Foreign.StablePtr (StablePtr, freeStablePtr, newStablePtr)
import Foreign (allocaArray)
import GHC.IO.FD (FD(..))
import GHC.IO.Handle.FD (fdToHandle)
import GHC.IO.Handle.Types (Handle(..), Handle__(..))
import Numeric (showHex)
import qualified System.IO as IO ()
import System.IO.Error (ioeSetErrorString)
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Bits (finiteBitSize)
#else
import Data.Bits (Bits, bitSize)

finiteBitSize :: (Bits a) => a -> Int
finiteBitSize = bitSize
#endif

#include <fcntl.h>
#include <windows.h>
##include "windows_cconv.h"

----------------------------------------------------------------
-- Platform specific definitions
--
-- Most typedefs and prototypes in Win32 are expressed in terms
-- of these types.  Try to follow suit - it'll make it easier to
-- get things working on Win64 (or whatever they call it on Alphas).
----------------------------------------------------------------

type BOOL          = Bool
type BYTE          = Word8
type UCHAR         = CUChar
type USHORT        = Word16
type UINT          = Word32
type INT           = Int32
type WORD          = Word16
type DWORD         = Word32
type LONG          = Int32
type FLOAT         = Float
type LARGE_INTEGER = Int64

type DWORD32       = Word32
type DWORD64       = Word64
type INT32         = Int32
type INT64         = Int64
type LONG32        = Int32
type LONG64        = Int64
type UINT32        = Word32
type UINT64        = Word64
type ULONG32       = Word32
type ULONG64       = Word64
type SHORT         = Int16

type DWORD_PTR     = Ptr DWORD32
type INT_PTR       = Ptr CInt
type ULONG         = Word32
type UINT_PTR      = Word
type LONG_PTR      = CIntPtr
type ULONG_PTR     = CUIntPtr
#ifdef _WIN64
type HALF_PTR      = Ptr INT32
#else
type HALF_PTR      = Ptr SHORT
#endif

-- Not really a basic type, but used in many places
type DDWORD        = Word64

----------------------------------------------------------------

type MbString      = Maybe String
type MbINT         = Maybe INT

type ATOM          = WORD
type WPARAM        = UINT_PTR
type LPARAM        = LONG_PTR
type LRESULT       = LONG_PTR
type SIZE_T        = ULONG_PTR

type MbATOM        = Maybe ATOM

type HRESULT       = LONG

----------------------------------------------------------------
-- Pointers
----------------------------------------------------------------

type Addr          = Ptr ()

type LPVOID        = Ptr ()
type LPBOOL        = Ptr BOOL
type LPBYTE        = Ptr BYTE
type PUCHAR        = Ptr UCHAR
type LPDWORD       = Ptr DWORD
type LPSTR         = Ptr CChar
type LPCSTR        = LPSTR
type LPWSTR        = Ptr CWchar
type LPCWSTR       = LPWSTR
type LPTSTR        = Ptr TCHAR
type LPCTSTR       = LPTSTR
type LPCTSTR_      = LPCTSTR

-- Optional things with defaults

maybePtr :: Maybe (Ptr a) -> Ptr a
maybePtr = fromMaybe nullPtr

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p = if p == nullPtr then Nothing else Just p

maybeNum :: Num a => Maybe a -> a
maybeNum = fromMaybe 0

numToMaybe :: (Eq a, Num a) => a -> Maybe a
numToMaybe n = if n == 0 then Nothing else Just n

type MbLPVOID      = Maybe LPVOID
type MbLPCSTR      = Maybe LPCSTR
type MbLPCTSTR     = Maybe LPCTSTR

----------------------------------------------------------------
-- Chars and strings
----------------------------------------------------------------

withTString    :: String -> (LPTSTR -> IO a) -> IO a
withTStringLen :: String -> ((LPTSTR, Int) -> IO a) -> IO a
peekTString    :: LPCTSTR -> IO String
peekTStringLen :: (LPCTSTR, Int) -> IO String
newTString     :: String -> IO LPCTSTR

-- UTF-16 version:
type TCHAR     = CWchar
withTString    = withCWString
withTStringLen = withCWStringLen
peekTString    = peekCWString
peekTStringLen = peekCWStringLen
newTString     = newCWString

{- ANSI version:
type TCHAR     = CChar
withTString    = withCString
withTStringLen = withCStringLen
peekTString    = peekCString
peekTStringLen = peekCStringLen
newTString     = newCString
-}

----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

type   HANDLE      = Ptr ()
type   ForeignHANDLE = ForeignPtr ()

newForeignHANDLE :: HANDLE -> IO ForeignHANDLE
newForeignHANDLE = newForeignPtr deleteObjectFinaliser

handleToWord :: HANDLE -> UINT_PTR
handleToWord = castPtrToUINTPtr

type   HKEY        = ForeignHANDLE
type   PKEY        = HANDLE

nullHANDLE :: HANDLE
nullHANDLE = nullPtr

type MbHANDLE      = Maybe HANDLE

nullHINSTANCE :: HINSTANCE
nullHINSTANCE = nullPtr

type   HINSTANCE   = Ptr ()
type MbHINSTANCE   = Maybe HINSTANCE

type   HMODULE     = Ptr ()
type MbHMODULE     = Maybe HMODULE

nullFinalHANDLE :: ForeignPtr a
nullFinalHANDLE = unsafePerformIO (newForeignPtr_ nullPtr)

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = castUINTPtrToPtr maxBound

foreign import ccall "_open_osfhandle"
  _open_osfhandle :: CIntPtr -> CInt -> IO CInt

-- | Create a Haskell 'Handle' from a Windows 'HANDLE'.
--
-- Beware that this function allocates a new file descriptor. A consequence of
-- this is that calling 'hANDLEToHandle' on the standard Windows handles will
-- not give you 'IO.stdin', 'IO.stdout', or 'IO.stderr'. For example, if you
-- run this code:
--
-- @
-- import Graphics.Win32.Misc
-- stdoutHANDLE <- getStdHandle sTD_OUTPUT_HANDLE
-- stdout2 <- 'hANDLEToHandle' stdoutHANDLE
-- @
--
-- Then although you can use @stdout2@ to write to standard output, it is not
-- the case that @'IO.stdout' == stdout2@.
hANDLEToHandle :: HANDLE -> IO Handle
hANDLEToHandle handle =
  _open_osfhandle (fromIntegral (ptrToIntPtr handle)) (#const _O_BINARY) >>= fdToHandle

foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- | Extract a Windows 'HANDLE' from a Haskell 'Handle' and perform
-- an action on it.

-- Originally authored by Max Bolingbroke in the ansi-terminal library
withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE haskell_handle action =
    -- Create a stable pointer to the Handle. This prevents the garbage collector
    -- getting to it while we are doing horrible manipulations with it, and hence
    -- stops it being finalized (and closed).
    withStablePtr haskell_handle $ const $ do
        -- Grab the write handle variable from the Handle
        let write_handle_mvar = case haskell_handle of
                FileHandle _ handle_mvar     -> handle_mvar
                DuplexHandle _ _ handle_mvar -> handle_mvar
                  -- This is "write" MVar, we could also take the "read" one

        -- Get the FD from the algebraic data type
        Just fd <- fmap (\(Handle__ { haDevice = dev }) -> fmap fdFD (cast dev))
                 $ readMVar write_handle_mvar

        -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
        windows_handle <- c_get_osfhandle fd

        -- Do what the user originally wanted
        action windows_handle

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

type ErrCode = DWORD

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
  v <- act
  if p v then errorWin wh else return ()

failIfNeg :: (Num a, Ord a) => String -> IO a -> IO a
failIfNeg = failIf (< 0)

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

eRROR_INSUFFICIENT_BUFFER :: ErrCode
eRROR_INSUFFICIENT_BUFFER = #const ERROR_INSUFFICIENT_BUFFER

eRROR_MOD_NOT_FOUND :: ErrCode
eRROR_MOD_NOT_FOUND = #const ERROR_MOD_NOT_FOUND

eRROR_PROC_NOT_FOUND :: ErrCode
eRROR_PROC_NOT_FOUND = #const ERROR_PROC_NOT_FOUND


errorWin :: String -> IO a
errorWin fn_name = do
  err_code <- getLastError
  failWith fn_name err_code

failWith :: String -> ErrCode -> IO a
failWith fn_name err_code = do
  c_msg <- getErrorMessage err_code
  msg <- if c_msg == nullPtr
           then return $ "Error 0x" ++ Numeric.showHex err_code ""
           else do msg <- peekTString c_msg
                   -- We ignore failure of freeing c_msg, given we're already failing
                   _ <- localFree c_msg
                   return msg
  -- turn GetLastError() into errno, which errnoToIOError knows how to convert
  -- to an IOException we can throw.
  errno <- c_maperrno_func err_code
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
      ioerror = errnoToIOError fn_name errno Nothing Nothing
                  `ioeSetErrorString` msg'
  throwIO ioerror


foreign import ccall unsafe "maperrno_func" -- in base/cbits/Win32Utils.c
   c_maperrno_func :: ErrCode -> IO Errno

----------------------------------------------------------------
-- Misc helpers
----------------------------------------------------------------

ddwordToDwords :: DDWORD -> (DWORD,DWORD)
ddwordToDwords n =
        (fromIntegral (n `shiftR` finiteBitSize (undefined :: DWORD))
        ,fromIntegral (n .&. fromIntegral (maxBound :: DWORD)))

dwordsToDdword:: (DWORD,DWORD) -> DDWORD
dwordsToDdword (hi,low) = (fromIntegral low) .|. (fromIntegral hi `shiftL` finiteBitSize hi)

-- Support for API calls that are passed a fixed-size buffer and tell
-- you via the return value if the buffer was too small.  In that
-- case, we double the buffer size and try again.
try :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO String
try loc f n = do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          r <- failIfZero loc $ f lptstr n
          if (r > n) then return (Left r) else do
            str <- peekTStringLen (lptstr, fromIntegral r)
            return (Right str)
   case e of
        Left n'   -> try loc f n'
        Right str -> return str

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

{-# CFILES cbits/HsWin32.c #-}
foreign import ccall "HsWin32.h &DeleteObjectFinaliser"
  deleteObjectFinaliser :: FunPtr (Ptr a -> IO ())

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

foreign import WINDOWS_CCONV unsafe "windows.h SetLastError"
  setLastError :: ErrCode -> IO ()

{-# CFILES cbits/errors.c #-}

foreign import ccall unsafe "errors.h"
  getErrorMessage :: DWORD -> IO LPWSTR

{-# CFILES cbits/HsWin32.c #-}

foreign import ccall unsafe "HsWin32.h"
  lOWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  hIWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  castUINTPtrToPtr :: UINT_PTR -> Ptr a

foreign import ccall unsafe "HsWin32.h"
  castPtrToUINTPtr :: Ptr s -> UINT_PTR

type LCID = DWORD

type LANGID = WORD
type SortID = WORD

foreign import ccall unsafe "HsWin32.h prim_MAKELCID"
  mAKELCID :: LANGID -> SortID -> LCID

foreign import ccall unsafe "HsWin32.h prim_LANGIDFROMLCID"
  lANGIDFROMLCID :: LCID -> LANGID

foreign import ccall unsafe "HsWin32.h prim_SORTIDFROMLCID"
  sORTIDFROMLCID :: LCID -> SortID

type SubLANGID = WORD
type PrimaryLANGID = WORD

foreign import ccall unsafe "HsWin32.h prim_MAKELANGID"
  mAKELANGID :: PrimaryLANGID -> SubLANGID -> LANGID

foreign import ccall unsafe "HsWin32.h prim_PRIMARYLANGID"
  pRIMARYLANGID :: LANGID -> PrimaryLANGID

foreign import ccall unsafe "HsWin32.h prim_SUBLANGID"
  sUBLANGID :: LANGID -> SubLANGID

----------------------------------------------------------------
-- End
----------------------------------------------------------------
