{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- A very bogus program (multiple errors) but
-- sent GHC 6.12 into a loop

module T3330a where

newtype Writer w a = Writer { runWriter :: (a, w) }
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

data AnyF (s :: * -> *) = AnyF
class HFunctor (f :: (* -> *) -> * -> *)
type family PF (phi :: * -> *) :: (* -> *) -> * -> *

children :: s ix -> (PF s) r ix -> [AnyF s]
children p x = execWriter (hmapM p collect x)

{-
0 from instantiating hmap
2 from instantiating collect

  (forall ixx. (phi0 ixx -> r0 ixx -> m0 (r'0 ixx) ~ s ix))
  phi0 ix0 ~ s2 ix2 -> r2 ix2 -> Writer [AnyF s2] (r2 ix2)
  f0 r0 ix0 ~ PF s r ix
  m0 (f0 r'0 ix0) ~ Writer [AnyF s] a0

Hence ix0 := ix
      r0  := r
      f0  := PF s
      phi0 := s2 ix2
      m0 := Writer [AnyF s]
      a0 : = f0 r'0 ix0

  (forall ixx. (s2 ix2 ixx -> r ixx -> Writer [AnyF s] (r'0 ixx) ~ s ix))
  s2 ix2 ix0 ~ s2 ix2 -> r2 ix2 -> Writer [AnyF s2] (r2 ix2)

-}

collect :: HFunctor (PF s) => s ix -> r ix -> Writer [AnyF s] (r ix)
collect = error "collect"

hmapM :: (forall ix. phi ix -> r ix -> m (r' ix))
      -> phi ix -> f r ix -> m (f r' ix)
hmapM = error "hmapM"

