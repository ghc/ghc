import Data.IORef
import GHC.IO
import Control.Exception

f :: Int -> IORef Bool -> IO ()
f x ref = do
  atomicWriteIORef ref True
  strictnessBarrier
  print $! x
{-# NOINLINE f #-}

main = do
  ref <- newIORef False
  catch (f (error "x") ref)
        (\(_ :: SomeException) ->  readIORef ref >>= print) -- should print True
