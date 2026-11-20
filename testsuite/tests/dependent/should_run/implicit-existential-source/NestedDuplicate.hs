{-# LANGUAGE LinearTypes #-}
module NestedDuplicate where

import Nested
import NestedSignature

bad :: PackedPair a %1 -> (PackedPair a, PackedPair a)
bad p = (roundTrip p, roundTrip p)
