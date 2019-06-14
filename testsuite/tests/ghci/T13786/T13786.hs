{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall unsafe "hello_a" helloA :: IO ()

