{-# LANGUAGE AllowAmbiguousTypes, DefaultSignatures, DerivingStrategies #-}

module T19692 where

-- Should not suggest enabling DeriveAnyClass
class C1 a where
  x1 :: a -> Int
data G1 = G1 deriving C1
data G1' = G1'
deriving instance C1 G1'

-- These should all suggest doing that
class C2 a
data G2 = G2 deriving C2
data G2' = G2'
deriving instance C2 G2'

class C3 a where
  x3 :: a -> Int
  x3 _ = 0
data G3 = G3 deriving C3
data G3' = G3'
deriving instance C3 G3'

class C4 a where
  x4 :: a -> Int
  default x4 :: Eq a => a -> Int
  x4 _ = 0
data G4 = G4 deriving C4
data G4' = G4'
deriving instance C4 G4'

-- These cases use a different code path. These ones should suggest enabling it:
class C5
deriving instance C5

class C6 a
deriving instance C6 a

-- These ones ideally shouldn't, but currently do:
class C7 a where
  x7 :: a -> Int
deriving instance C7 a

class C8 where
  x8 :: Int
deriving instance C8

-- "Making an instance for a typeclass" is also handled specially. Should
-- suggest:
class C9 a
deriving instance C9 Eq

-- Should not suggest:
class C10 a where
  x10 :: a Int => Int
deriving instance C10 Eq

-- And "anyclass specifically asked for" is different again. We want to suggest
-- even if it would generate a warning.
data G11 = G11 Int deriving anyclass Eq
data G11' = G11' Int
deriving anyclass instance Eq G11'
