-- |
-- Module      : Crypto.Random.Entropy.Windows
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Code originally from the entropy package and thus is:
--   Copyright (c) Thomas DuBuisson.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Crypto.Random.Entropy.Windows
    ( WinCryptoAPI
    ) where

import Data.Int (Int32)
import Data.Word
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (peek)
import System.Win32.Types (getLastError)

import Crypto.Random.Entropy.Source


-- | Handle to Windows crypto API for random generation
data WinCryptoAPI = WinCryptoAPI

instance EntropySource WinCryptoAPI where
    entropyOpen = do
        mctx <- cryptAcquireCtx
        maybe (return Nothing) (\ctx -> cryptReleaseCtx ctx >> return (Just WinCryptoAPI)) mctx
    entropyGather WinCryptoAPI ptr n = do
        mctx <- cryptAcquireCtx
        case mctx of
            Nothing  -> do
                lastError <- getLastError
                fail $ "cannot re-grab win crypto api: error " ++ show lastError
            Just ctx -> do
                r <- cryptGenRandom ctx ptr n
                cryptReleaseCtx ctx
                return r
    entropyClose WinCryptoAPI = return ()


type DWORD = Word32
type BOOL  = Int32
type BYTE  = Word8

#if defined(ARCH_X86)
# define WINDOWS_CCONV stdcall
type CryptCtx = Word32
#elif defined(ARCH_X86_64)
# define WINDOWS_CCONV ccall
type CryptCtx = Word64
#else
# error Unknown mingw32 arch
#endif

-- Declare the required CryptoAPI imports
foreign import WINDOWS_CCONV unsafe "CryptAcquireContextA"
   c_cryptAcquireCtx :: Ptr CryptCtx -> CString -> CString -> DWORD -> DWORD -> IO BOOL
foreign import WINDOWS_CCONV unsafe "CryptGenRandom"
   c_cryptGenRandom :: CryptCtx -> DWORD -> Ptr BYTE -> IO BOOL
foreign import WINDOWS_CCONV unsafe "CryptReleaseContext"
   c_cryptReleaseCtx :: CryptCtx -> DWORD -> IO BOOL


-- Define the constants we need from WinCrypt.h
msDefProv :: String
msDefProv = "Microsoft Base Cryptographic Provider v1.0"

provRSAFull :: DWORD
provRSAFull = 1

cryptVerifyContext :: DWORD
cryptVerifyContext = 0xF0000000

cryptAcquireCtx :: IO (Maybe CryptCtx)
cryptAcquireCtx =
    alloca $ \handlePtr ->
    withCString msDefProv $ \provName -> do
        r <- toBool `fmap` c_cryptAcquireCtx handlePtr nullPtr provName provRSAFull cryptVerifyContext
        if r
            then Just `fmap` peek handlePtr
            else return Nothing

cryptGenRandom :: CryptCtx -> Ptr Word8 -> Int -> IO Int
cryptGenRandom h buf n = do
    success <- toBool `fmap` c_cryptGenRandom h (fromIntegral n) buf
    return $ if success then n else 0

cryptReleaseCtx :: CryptCtx -> IO ()
cryptReleaseCtx h = do
    success <- toBool `fmap` c_cryptReleaseCtx h 0
    if success
        then return ()
        else do
            lastError <- getLastError
            fail $ "cryptReleaseCtx: error " ++ show lastError
