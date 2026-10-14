{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T27424a where

class C a
class D a
class E a

data G a where
  MkCurried :: C a => D a => E a => G a      -- multiple consecutive contexts
  MkConjoined :: (C a, D a, E a) => G a      -- a single context, accepted before #27424 too
  MkMixedL :: (C a, D a) => E a => G a
  MkMixedR :: C a => (D a, E a) => G a

-- Curried contexts are equivalent to a single conjoined context,
-- as witnessed by this involution.
isoG :: G a -> G a
isoG MkCurried = MkConjoined
isoG MkConjoined = MkCurried
isoG MkMixedL = MkMixedR
isoG MkMixedR = MkMixedL

-- Matching on either constructor brings all three constraints into scope.
withG :: G a -> ((C a, D a, E a) => r) -> r
withG MkCurried k = k
withG MkConjoined k = k
withG MkMixedL k = k
withG MkMixedR k = k

data H a where
  MkH1 :: C a => D a => E a => H a
  MkH2 :: (C a) => (D a) => (E a) => H a
  MkH3 :: forall a. C a => D a => H a
  MkH4 :: (forall a. C a => D a => H a)
  MkH5 :: forall a -> C a => D a => H a
  MkH6 :: () => () => H a
  MkH7 :: C a => D a => a -> Int -> H a
  MkH8 :: (C a, D a) => E a => H a
  MkH9 :: C a => (D a, E a) => H a
  -- MkH10 :: C a => (D a => H a)                           -- Uncomment later (#18389)
  -- MkH11 :: forall a. C a => forall b. D b => b -> H a    -- Uncomment later (#18389)
  -- MkH12 :: C a => Int -> D a => H a                      -- Uncomment later (#18389)

gToH1 :: G a -> H a
gToH1 MkCurried = MkH1
gToH1 MkConjoined = MkH1
gToH1 MkMixedL = MkH1
gToH1 MkMixedR = MkH1
