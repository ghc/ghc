{-# LANGUAGE DataKinds, KindSignatures #-}

import GHC.TypeLits (Nat, Symbol)

class A (n::Nat)
instance A 0

class B (s::Symbol)
instance B "B"

main :: IO ()
main = return ()
