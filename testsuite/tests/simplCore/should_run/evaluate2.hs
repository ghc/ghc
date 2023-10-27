import Data.IORef
import GHC.IO
import Debug.Trace

f :: Int -> Int -> IO ()
f x y = do
  evaluate2 y
  print $! x
{-# NOINLINE f #-}

main = f (trace "x" 0) (trace "y" 1) -- should print y before x
