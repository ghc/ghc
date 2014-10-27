{-# LANGUAGE NoImplicitPrelude #-}

-- | /Since: 4.7.0.0/
module GHC.Profiling where

import GHC.Base

foreign import ccall startProfTimer :: IO ()
foreign import ccall stopProfTimer :: IO ()
