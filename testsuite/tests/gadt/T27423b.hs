{-# LANGUAGE GADTs #-}
module T27423b where

-- Record-style GADT constructors must remain unparenthesisable, per
-- GHC Proposal #402: this is out of scope for #27423 and should
-- continue to be rejected.
data T1 a where
  MkT1 :: ({ fld :: a } -> T1 a)

data T2 a where
  MkT2 :: (forall a. { fld :: a } -> T2 a)

-- Without parentheses, forall-or-nothing applies to the whole type, so
-- `b` is not implicitly quantified and this must be rejected.
data T3 where
  MkT3 :: forall a. a -> b -> T3
