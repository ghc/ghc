-- !!! Testing callbacks
module Main(main) where

import IOExts
import Addr

count :: IORef Int -> IO Int
count ref = do
  x <- readIORef ref
  writeIORef ref (x+1)
  return x

createCounter :: IO Addr
createCounter = do
  ref <- newIORef 0
  mkCounter (count ref)

foreign export dynamic mkCounter :: (IO Int) -> IO Addr

main :: IO ()
main = do
  x  <- createCounter
  v1 <- _casm_GC_ `` do { typedef HsInt (*f)(); %r=(HsInt)((f)%0)();} while (0); '' x
  print (v1::Int)
  v2 <- _casm_GC_ `` do { typedef HsInt (*f)(); %r=(HsInt)((f)%0)();} while (0); '' x
  print (v2::Int)

