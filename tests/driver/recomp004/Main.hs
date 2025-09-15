
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

main :: IO ()
main = foo

foreign import ccall unsafe "c.h foo" foo :: IO ()

