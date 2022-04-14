{-# OPTIONS_GHC -O0 -fobject-code #-}

module Lib (Ty(..)) where

-- We need to ensure that this *isn't* unpacked.
data Ty = Ty {-# NOUNPACK #-} !(Maybe Int)

