{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall unsafe "test" test :: IO ()

main :: IO ()
main = test
