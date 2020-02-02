{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, DataKinds, TypeFamilies #-}
module T9569 where

import GHC.Exts

data Proxy (c :: Constraint)

class Deferrable (c :: Constraint) where
  defer :: Proxy c -> (c => a) -> a

deferPair :: (Deferrable c1, Deferrable c2)
          => Proxy (c1,c2) -> (((c1,c2) :: Constraint) => a) -> a
             -- NB: ((c1,c2) :: Constraint) => blah
             -- is different form
             --     (c1,c2) => blah
             -- The former has dict, the latter has two
deferPair _ _ = undefined

instance (Deferrable c1, Deferrable c2) => Deferrable (c1,c2) where
    -- defer p f = deferPair p f     -- Succeeds
    defer = deferPair                -- Fails

{- Notes Apr 2020.
~~~~~~~~~~~~~~~~~
Note the careful type for deferPair!  You can also say

deferPair :: (Deferrable c1, Deferrable c2, d ~ (c1,c2))
          => Proxy (c1,c2) -> (d => a) -> a

but NOT

deferPair :: (Deferrable c1, Deferrable c2)
          => Proxy (c1,c2) -> ((c1,c2) => a) -> a

The point is that
  (c1,c2) => a
is short for
  c1 => c2 => a
-}

{-
  [G] Deferrable c1, Deferrable c2

  [W] Proxy (c1,c2)   -> ((c1,c2) => a) -> a
        ~
      Proxy (c1x,c2x) -> ((c1x,c2x) => ax) -> ax
  [w] Deferrable c1x
  [w] Deferrable c2x
-}
