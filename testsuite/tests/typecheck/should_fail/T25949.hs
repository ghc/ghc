module T25949 where

import Data.Coerce

import T25949_aux
  ( OpaqueNT, OutOfScopeNT
  , FamNT, Opaque, OutOfScope
  )

testOpaque :: OpaqueNT -> Int
testOpaque = coerce

testOutOfScope :: OutOfScopeNT -> Int
testOutOfScope = coerce

testOpaqueFam :: FamNT Opaque -> Int
testOpaqueFam = coerce

testOutOfScopeFam :: FamNT OutOfScope -> Int
testOutOfScopeFam = coerce
