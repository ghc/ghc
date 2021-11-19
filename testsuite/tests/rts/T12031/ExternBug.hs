-- Copyright (c) 2016, Ryan Scott
-- ExternBug.hs
{-# LANGUAGE ForeignFunctionInterface #-}
module ExternBug (bar) where

foreign import ccall "bar"
  bar :: IO ()
