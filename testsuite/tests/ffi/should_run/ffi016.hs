-- Tests Foreign.Concurrent finalizers

import Text.Printf
import Foreign.Concurrent as Conc
import Foreign
import GHC.TopHandler
import Control.Concurrent
import Data.List
import System.Mem

-- This finalizer calls back into Haskell, so we can't use
-- the ordinary newForeignPtr.
foreign export ccall fin :: Ptr Int -> Ptr Int -> IO ()
foreign import ccall "fin" finptr :: Ptr Int -> Ptr Int -> IO ()

fin :: Ptr Int -> Ptr Int -> IO ()
fin envp ap = runIO $ do
  env <- peek envp
  a <- peek ap
  printf "%d %d\n" env a
  return ()

main = do
  a   <- new (55 :: Int)
  env <- new (66 :: Int)
  fp  <- Conc.newForeignPtr a (finptr env a)
  performGC
  threadDelay 100000
