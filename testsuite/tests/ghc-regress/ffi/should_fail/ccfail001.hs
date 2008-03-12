{-# OPTIONS -fffi -fglasgow-exts #-}

-- Trac #1037 

module Foo where

import GHC.Prim

foreign import ccall foo :: Int -> State# RealWorld
