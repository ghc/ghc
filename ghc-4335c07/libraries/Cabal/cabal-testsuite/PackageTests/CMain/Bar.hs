{-# LANGUAGE ForeignFunctionInterface #-}
module Bar where

bar :: IO ()
bar = return ()

foreign export ccall bar :: IO ()
