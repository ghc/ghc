{-# LANGUAGE CPP #-}

module EventUtil
    (
      setNonBlocking
    , throwErrnoIfMinus1Retry_mayBlock
    , throwErrnoIfMinus1Retry_repeatOnBlock
    ) where

import Foreign.C.Error (eINTR, eWOULDBLOCK, eAGAIN, getErrno, throwErrno)
import Foreign.C.Types (CInt)
import Prelude hiding (repeat)
import System.Posix.Internals (setNonBlockingFD)
import System.Posix.Types (Fd)

{-# SPECIALISE
    throwErrnoIfMinus1Retry_mayBlock
         :: String -> IO CInt -> IO CInt -> IO CInt #-}
throwErrnoIfMinus1Retry_mayBlock :: (Eq a, Num a) => String ->
                                    IO a -> IO a -> IO a
throwErrnoIfMinus1Retry_mayBlock name on_block act = do
    res <- act
    if res == -1
        then do
            err <- getErrno
            if err == eINTR
                then throwErrnoIfMinus1Retry_mayBlock name on_block act
                else if err == eWOULDBLOCK || err == eAGAIN
                        then on_block
                        else throwErrno name
        else return res

throwErrnoIfMinus1Retry_repeatOnBlock :: (Eq a, Num a) => String ->
                                         IO b -> IO a -> IO a
throwErrnoIfMinus1Retry_repeatOnBlock name on_block act =
  throwErrnoIfMinus1Retry_mayBlock name (on_block >> repeat) act
  where repeat = throwErrnoIfMinus1Retry_repeatOnBlock name on_block act

setNonBlocking :: Fd -> IO ()
setNonBlocking fd =
#if __GLASGOW_HASKELL__ > 611
    setNonBlockingFD (fromIntegral fd) True
#else
    setNonBlockingFD (fromIntegral fd)
#endif
