{-# LANGUAGE GADTs, LinearTypes #-}
module NestedDuplicate where

import Nested
import NestedSignature

bad :: PackedPair a %1 -> (PackedPair a, PackedPair a)
bad p = (roundTrip p, roundTrip p)

badEquation :: HiddenUnit %1 -> ((), ())
badEquation (Unit u) = (u, u)
