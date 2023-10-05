{-# OPTIONS_GHC -fdefer-type-errors #-}
module T15325 where

class PolyList e where
     polyList :: e -> ()

f :: PolyList e => e -> ()
f x = polyList x

plh :: ()
plh = f 0
