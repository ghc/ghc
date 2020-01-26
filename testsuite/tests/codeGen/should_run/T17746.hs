module Main where

import Foreign.ForeignPtr
import Control.Concurrent
import Control.Monad
import Data.Word
import Foreign.Storable
import System.Mem

main :: IO ()
main = do
    fp <- mallocForeignPtrBytes 40000
    _ <- forkIO $ do
       -- withForeignPtr is supposed to keep `p` alive but it wasn't the case
       withForeignPtr fp $ \p -> do
         forever $ do
           performGC
           -- We need to perform at least two operations here on the pointer.
           -- By doing so, the loop worker takes an Addr# as a parameter.
           -- Otherwise `byteArrayContents` is used here (instead of in the
           -- caller) and the ByteArray used by the ForeignPtr implementation is
           -- kept alive even with `touch#` removed.
           poke p (0xDEADBEEF :: Word32)
           poke p (0x12345678 :: Word32)
           --touchForeignPtr fp
           yield
    threadDelay 1000
