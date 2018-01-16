
{-# OPTIONS_GHC -fwarn-unused-binds #-}

module A (a, a_type_default, a_missing_sig) where

a :: Int
a = 4

a_type_default :: Int
a_type_default = 2 ^ 2

a_missing_sig = ()

