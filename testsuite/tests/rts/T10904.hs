import Control.Concurrent
import Control.Monad
import Foreign
import Foreign.C.Types
import System.Environment


foreign import ccall safe "finalizerlib.h init_value"
    init_value :: Ptr CInt -> IO ()

foreign import ccall safe "finalizerlib.h &finalize_value"
    finalize_value :: FinalizerPtr CInt


allocateValue :: IO ()
allocateValue = do
    fp <- mallocForeignPtrBytes 10000
    withForeignPtr fp init_value
    addForeignPtrFinalizer finalize_value fp


main :: IO ()
main = do
    [n] <- fmap (fmap read) getArgs
    _ <- forkIO (loop n)
    loop n
  where
    loop n = replicateM_ n allocateValue
