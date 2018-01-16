{-# LANGUAGE ForeignFunctionInterface #-}

-- !!! returning a Bool from a foreign export confused GHCi 6.0.1.

import Foreign

foreign import ccall "wrapper" 
  mkFoo :: (Int -> IO Bool) -> IO (FunPtr (Int -> IO Bool))

foo :: Int -> IO Bool
foo x = return (x == 42)

foreign import ccall "dynamic"
  call_foo :: FunPtr (Int -> IO Bool) -> Int -> IO Bool

main = do
   foo_fun <- mkFoo foo
   call_foo foo_fun 3 >>= print
   call_foo foo_fun 42 >>= print
