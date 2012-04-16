{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, BangPatterns, ForeignFunctionInterface, CApiFFI #-}

module GHC.Event.Clock (getMonotonicTime, initializeTimer) where

#include "HsBase.h"

import Foreign
import Foreign.C.Types
import GHC.Base
import GHC.Real

#if !darwin_HOST_OS
import Foreign.C.Error (throwErrnoIfMinus1_)
import GHC.Err
import GHC.Num
#endif

-- TODO: Implement this for Windows.

initializeTimer :: IO ()

-- | Return monotonic time in seconds, since some unspecified starting point
getMonotonicTime :: IO Double

------------------------------------------------------------------------
-- FFI binding

#if HAVE_CLOCK_GETTIME

initializeTimer = return ()

getMonotonicTime = do
    tv <- with (CTimespec 0 0) $ \tvptr -> do
        throwErrnoIfMinus1_ "clock_gettime" (clock_gettime (#const CLOCK_ID) tvptr)
        peek tvptr
    let !t = realToFrac (sec tv) + realToFrac (nsec tv) / 1000000000.0
    return t

data CTimespec = CTimespec
    { sec  :: {-# UNPACK #-} !CTime
    , nsec :: {-# UNPACK #-} !CLong
    }

instance Storable CTimespec where
    sizeOf _ = #size struct timespec
    alignment _ = alignment (undefined :: CLong)

    peek ptr = do
        sec' <- #{peek struct timespec, tv_sec} ptr
        nsec' <- #{peek struct timespec, tv_nsec} ptr
        return $ CTimespec sec' nsec'

    poke ptr tv = do
        #{poke struct timespec, tv_sec} ptr (sec tv)
        #{poke struct timespec, tv_nsec} ptr (nsec tv)

foreign import capi unsafe "HsBase.h clock_gettime" clock_gettime
    :: Int -> Ptr CTimespec -> IO CInt

#elif darwin_HOST_OS

getMonotonicTime = do
    with 0.0 $ \timeptr -> do
    absolute_time timeptr
    ctime <- peek timeptr
    let !time = realToFrac ctime
    return time

foreign import capi unsafe "HsBase.h absolute_time" absolute_time ::
    Ptr CDouble -> IO ()

foreign import capi unsafe "HsBase.h initialize_timer"
  initializeTimer :: IO ()

#else

initializeTimer = return ()

getMonotonicTime = do
    tv <- with (CTimeval 0 0) $ \tvptr -> do
        throwErrnoIfMinus1_ "gettimeofday" (gettimeofday tvptr nullPtr)
        peek tvptr
    let !t = realToFrac (sec tv) + realToFrac (usec tv) / 1000000.0
    return t

data CTimeval = CTimeval
    { sec  :: {-# UNPACK #-} !CTime
    , usec :: {-# UNPACK #-} !CSUSeconds
    }

instance Storable CTimeval where
    sizeOf _ = #size struct timeval
    alignment _ = alignment (undefined :: CLong)

    peek ptr = do
        sec' <- #{peek struct timeval, tv_sec} ptr
        usec' <- #{peek struct timeval, tv_usec} ptr
        return $ CTimeval sec' usec'

    poke ptr tv = do
        #{poke struct timeval, tv_sec} ptr (sec tv)
        #{poke struct timeval, tv_usec} ptr (usec tv)

foreign import capi unsafe "HsBase.h gettimeofday" gettimeofday
    :: Ptr CTimeval -> Ptr () -> IO CInt

#endif
