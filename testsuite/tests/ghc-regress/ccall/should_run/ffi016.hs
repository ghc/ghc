-- Tests ForeignPtrEnv finalizers

import Text.Printf
import Foreign.ForeignPtr
import Foreign
import GHC.TopHandler
import Control.Concurrent
import Data.List

foreign export ccall fin :: Ptr Int -> Ptr Int -> IO ()
foreign import ccall "&fin" finptr :: FinalizerEnvPtr Int Int

fin :: Ptr Int -> Ptr Int -> IO ()
fin envp ap = runIO $ do
  env <- peek envp
  a <- peek ap
  printf "%d %d\n" env a
  return ()

main = do
  a   <- new (55 :: Int)
  env <- new (66 :: Int)
  fp  <- newForeignPtrEnv finptr env a
  foldl' (+) 0 [1..500000] `seq` return ()
