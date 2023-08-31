{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

foreign import ccall unsafe "test" main :: IO ()
