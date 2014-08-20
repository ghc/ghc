{-# LANGUAGE ForeignFunctionInterface #-}
module B042stub.C where

foreign export ccall foo :: IO ()
foo :: IO ()
foo = return ()
