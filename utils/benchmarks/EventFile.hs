{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- | File functions using System.Event instead of GHC's I/O manager.
module EventFile
    (
      read
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Error (eINTR, getErrno, throwErrno)
#if __GLASGOW_HASKELL__ < 612
import GHC.IOBase (IOErrorType(..))
#else
import GHC.IO.Exception (IOErrorType(..))
#endif
#if defined(USE_GHC_IO_MANAGER)
import GHC.Conc (threadWaitRead)
#else
import System.Event.Thread (threadWaitRead)
#endif
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.Posix.Internals (c_read)
import System.Posix.Types (Fd)
import Prelude hiding (read)
import EventUtil

read :: Fd -> Int -> IO ByteString
read fd nbytes
    | nbytes <= 0 = ioError (mkInvalidReadArgError "read")
    | otherwise   = createAndTrim nbytes $ readInner fd nbytes

readInner :: Fd -> Int -> Ptr Word8 -> IO Int
readInner fd nbytes ptr = do
    len <- throwErrnoIfMinus1Retry_repeatOnBlock "read"
           (threadWaitRead (fromIntegral fd)) $
           c_read (fromIntegral fd) (castPtr ptr) (fromIntegral nbytes)
    case fromIntegral len of
         (-1) -> do errno <- getErrno
                    if errno == eINTR
                       then readInner fd nbytes ptr
                       else throwErrno "read"
         n -> return n

mkInvalidReadArgError :: String -> IOError
mkInvalidReadArgError loc = ioeSetErrorString (mkIOError InvalidArgument
                                               loc Nothing Nothing)
                            "non-positive length"
