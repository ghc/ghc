{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

foreign import ccall "global.h printx"
  c_printx :: IO ()

main = do
  c_printx
