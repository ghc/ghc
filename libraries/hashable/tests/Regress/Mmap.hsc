{-# LANGUAGE CApiFFI #-}

module Regress.Mmap (regressions) where

#include <sys/mman.h>

import Control.Exception (bracket, evaluate)
import Control.Monad (forM_)
import Data.Bits ((.|.))
import Data.ByteString.Internal (ByteString(..))
import Data.Hashable (hash)
import Foreign.C.Error (throwErrnoIf, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, intPtrToPtr, nullPtr, plusPtr)
import GHC.ForeignPtr (newForeignPtr_)
import System.Posix.Types (COff(..))
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import qualified Data.ByteString as B

withMapping :: (Ptr a -> Int -> IO ()) -> IO ()
withMapping go = do
  pageSize <- fromIntegral `fmap` getPageSize
  let mappingSize = pageSize * 2
  bracket (mmap
           nullPtr
           mappingSize
           ((#const PROT_READ) .|. (#const PROT_WRITE))
           ((#const MAP_ANON) .|. (#const MAP_PRIVATE))
           (-1)
           0)
           (flip munmap mappingSize) $ \mappingPtr -> do
    go mappingPtr (fromIntegral pageSize)
    mprotect (mappingPtr `plusPtr` fromIntegral pageSize)
             pageSize (#const PROT_NONE)

hashNearPageBoundary :: IO ()
hashNearPageBoundary =
  withMapping $ \ptr pageSize -> do
    let initialSize = 16
    fp <- newForeignPtr_ (ptr `plusPtr` (pageSize - initialSize))
    let bs0 = PS fp 0 initialSize
    forM_ (B.tails bs0) $ \bs -> do
      evaluate (hash bs)

regressions :: [Test]
regressions = [
   testCase "hashNearPageBoundary" hashNearPageBoundary
 ]

mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)
mmap addr len prot flags fd offset =
    throwErrnoIf (== intPtrToPtr (#const MAP_FAILED)) "mmap" $
    c_mmap addr len prot flags fd offset

munmap :: Ptr a -> CSize -> IO CInt
munmap addr len = throwErrnoIfMinus1 "munmap" $ c_munmap addr len

mprotect :: Ptr a -> CSize -> CInt -> IO ()
mprotect addr len prot =
    throwErrnoIfMinus1_ "mprotect" $ c_mprotect addr len prot

foreign import capi unsafe "sys/mman.h mmap"
    c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import capi unsafe "sys/mman.h munmap"
    c_munmap :: Ptr a -> CSize -> IO CInt

foreign import capi unsafe "sys/mman.h mprotect"
    c_mprotect :: Ptr a -> CSize -> CInt -> IO CInt

foreign import capi unsafe "unistd.h getpagesize"
    getPageSize :: IO CInt
