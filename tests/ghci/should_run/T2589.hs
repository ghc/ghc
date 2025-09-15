{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Ptr
import GHC.Exts
import Foreign.Ptr

main = print (Ptr nullAddr# == nullPtr)
