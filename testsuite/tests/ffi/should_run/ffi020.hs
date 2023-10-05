import Foreign
import Data.Word
import Control.Monad
import System.Mem

main = do
  let finalize p = do
        putStrLn ("finalize: " ++ show p)
        free p
  allocToForeignPtr finalize (mallocBytes 4096)
  forever performGC

allocToForeignPtr :: (Ptr a -> IO ())    -- finalizer
                  -> IO (Ptr a)          -- allocate
                  -> IO (ForeignPtr a)
allocToForeignPtr fin alloc = do
  done <- asFinalizer fin
  newForeignPtr done =<< alloc

asFinalizer :: (Ptr a -> IO ()) -> IO (FinalizerPtr a)
asFinalizer = mkFinalizer
foreign import ccall "wrapper"
  mkFinalizer :: (Ptr a -> IO ())
              -> IO (FinalizerPtr a)

