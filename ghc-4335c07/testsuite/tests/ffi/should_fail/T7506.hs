module Foo where

import Foreign.Ptr

-- foreign import ccall "stdio.h &putchar" c_putchar :: () -> FunPtr (Char -> IO ())
foreign import ccall "stdio.h &putchar" c_putchar :: Int -> IO ()
