module T22696c where

class C a where
  m :: a

data S1 c a where
  MkS1 :: c => S1 c a
instance c => C (S1 c a) where
  m = MkS1

data S2 c a where
  MkS2 :: c a => S2 c a
instance c a => C (S2 c a) where
  m = MkS2

newtype T1 c a = MkT1 (S1 c a) deriving C
newtype T2 c a = MkT2 (S2 c a) deriving C
  -- The inferred instances would be:
  --
  --  instance c   => C (T1 c a)
  --  instance c a => C (T2 c a)
  --
  -- These are valid instance context for the reasons described in
  -- (VD2) in Note [Valid 'deriving' predicate] in GHC.Tc.Validity.
