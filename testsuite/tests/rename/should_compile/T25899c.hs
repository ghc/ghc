{-# LANGUAGE ExplicitNamespaces #-}

module T25899c where

import T25899c_helper (T(..))   -- imports T, P and f

g :: T -> Int
g (P {f = x}) = x
