{-# LANGUAGE GADTs #-}

module T27424b where

class C a
class D a

-- Record-style GADT constructors allow at most one context, per
-- GHC Proposal #402, so curried contexts must remain rejected there.
data T1 a where
  MkT1 :: C a => D a => { fld1 :: a } -> T1 a

data T2 a where
  MkT2 :: forall a. (C a) => (D a) => { fld2 :: a } -> T2 a

data T3 a where
  MkT3 :: () => () => { fld3 :: a } -> T3 a
