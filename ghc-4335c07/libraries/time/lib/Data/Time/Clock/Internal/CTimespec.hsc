module Data.Time.Clock.Internal.CTimespec where

#include "HsTimeConfig.h"

#if !defined(mingw32_HOST_OS) && HAVE_CLOCK_GETTIME

#if __GLASGOW_HASKELL__ >= 709
import Foreign
#else
import Foreign.Safe
#endif
import Foreign.C
import System.IO.Unsafe

#include <time.h>

type ClockID = #{type clockid_t}

data CTimespec = MkCTimespec CTime CLong

instance Storable CTimespec where
    sizeOf _ = #{size struct timespec}
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s  <- #{peek struct timespec, tv_sec } p
        ns <- #{peek struct timespec, tv_nsec} p
        return (MkCTimespec s ns)
    poke p (MkCTimespec s ns) = do
        #{poke struct timespec, tv_sec } p s
        #{poke struct timespec, tv_nsec} p ns

foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: ClockID -> Ptr CTimespec -> IO CInt
foreign import ccall unsafe "time.h clock_getres"
    clock_getres :: ClockID -> Ptr CTimespec -> IO CInt

-- | Get the resolution of the given clock.
clockGetRes :: #{type clockid_t} -> IO (Either Errno CTimespec)
clockGetRes clockid = alloca $ \ptspec -> do
    rc <- clock_getres clockid ptspec
    case rc of
        0 -> do
            res <- peek ptspec
            return $ Right res
        _ -> do
            errno <- getErrno
            return $ Left errno

-- | Get the current time from the given clock.
clockGetTime :: ClockID -> IO CTimespec
clockGetTime clockid = alloca (\ptspec -> do
    throwErrnoIfMinus1_ "clock_gettime" $ clock_gettime clockid ptspec
    peek ptspec
    )

clock_REALTIME :: ClockID
clock_REALTIME = #{const CLOCK_REALTIME}

clock_TAI :: ClockID
clock_TAI = #{const 11}

realtimeRes :: CTimespec
realtimeRes = unsafePerformIO $ do
    mres <- clockGetRes clock_REALTIME
    case mres of
        Left errno -> ioError (errnoToIOError "clock_getres" errno Nothing Nothing)
        Right res -> return res

clockResolution :: ClockID -> Maybe CTimespec
clockResolution clockid = unsafePerformIO $ do
    mres <- clockGetRes clockid
    case mres of
        Left _ -> return Nothing
        Right res -> return $ Just res

#endif
