-- Copyright (c) 2016, Ryan Scott
-- ExternBug.hs
{-# LANGUAGE ForeignFunctionInterface #-}
module ExternBug (bar) where

{-# INCLUDE foo.h #-}

foreign import ccall "bar"
  bar :: IO ()
