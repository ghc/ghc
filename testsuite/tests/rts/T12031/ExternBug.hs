-- Copyright (c) 2016, Ryan Scott
-- ExternBug.hs
--
module ExternBug (bar) where

foreign import ccall "bar"
  bar :: IO ()
