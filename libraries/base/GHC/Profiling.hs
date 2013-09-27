{-# LANGUAGE ForeignFunctionInterface #-}

-- | /Since: 4.7.0.0/
module GHC.Profiling where

foreign import ccall startProfTimer :: IO ()
foreign import ccall stopProfTimer :: IO ()
