
{-# LANGUAGE ForeignFunctionInterface, MagicHash #-}

-- #1037 

module Foo where

import GHC.Exts

foreign import ccall foo :: Int -> State# RealWorld
