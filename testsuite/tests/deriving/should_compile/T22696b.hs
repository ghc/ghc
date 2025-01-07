module T22696b where

class C a where
  m :: a

data S c where
  MkS :: c Int => S c

instance c Int => C (S c) where
  m = MkS

newtype T c = MkT (S c)
  deriving C
  -- The inferred instance is:
  --
  --  instance c Int => C (T c)
