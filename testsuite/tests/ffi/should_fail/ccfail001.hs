
{-# LANGUAGE ForeignFunctionInterface, MagicHash #-}

-- Trac #1037 

module Foo where

import GHC.Prim

foreign import ccall foo :: Int -> State# RealWorld
