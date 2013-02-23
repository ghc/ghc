{-# LANGUAGE ForeignFunctionInterface #-}

module GHC.Profiling where

foreign import ccall startProfTimer :: IO ()
foreign import ccall stopProfTimer :: IO ()
