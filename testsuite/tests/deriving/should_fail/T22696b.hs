module T22696b where

class C a where
  m :: a

data S c where
  MkS :: c Int => S c

instance c Int => C (S c) where
  m = MkS

newtype T c = MkT (S c)
  deriving C
  -- The inferred instance would be:
  --
  --  instance c Int => C (T c)
  --
  -- And we want to reject this instance due to the reasons mentioned in
  -- (VD2) in Note [Valid 'deriving' predicate] in GHC.Tc.Validity.
