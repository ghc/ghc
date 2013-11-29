{-# OPTIONS_GHC -fplugin StrAnalAnnotation #-}

-- Just an example on how to create tests that test the strictness analizer

module StrAnalExample where

import StrAnalAnnotation (StrAnal(StrAnal))

foo x = x
{-# ANN foo (StrAnal "<S,1*U>") #-}
