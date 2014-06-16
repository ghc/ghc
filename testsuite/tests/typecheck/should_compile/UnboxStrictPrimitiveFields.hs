{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -funbox-small-strict-fields #-}
module Test where

import GHC.Exts

-- The following should be unboxed automatically:
data A = A Int#
data B = B !A
data C = C !()
data D = D !B
data E = E !D !D

d = D (B (A 0#))
e = E d d

data F = F Int#
newtype G = G F
data H = H !G !G

h = H (G (F 1#)) (G (F 1#))

-- This should not be unboxed:

data I = I !H !H

i = I h h

