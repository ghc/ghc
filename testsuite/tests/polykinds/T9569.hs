{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, DataKinds, TypeFamilies #-}
module T9569 where

import GHC.Exts

data Proxy (c :: Constraint)

class Deferrable (c :: Constraint) where
  defer :: Proxy c -> (c => a) -> a

deferPair :: (Deferrable c1, Deferrable c2)
          => Proxy (c1,c2) -> ((c1,c2) => a) -> a
deferPair _ _ = undefined

instance (Deferrable c1, Deferrable c2) => Deferrable (c1,c2) where
    -- defer p f = deferPair p f     -- Succeeds
    defer = deferPair                -- Fails

{- Notes Apr 2020.
~~~~~~~~~~~~~~~~~
Wow, this is horrible! If you say

deferPair :: (Deferrable c1, Deferrable c2, d ~ (c1,c2))
          => Proxy (c1,c2) -> (d => a) -> a
then it all works.  The point is that
  (c1,c2) => a
is short for
  c1 => c2 => a
and we have no source language notation for the form that takes
a single constraint tuple.
-}

{-
  [G] Deferrable c1, Deferrable c2

  [W] Proxy (c1,c2)   -> ((c1,c2) => a) -> a
        ~
      Proxy (c1x,c2x) -> ((c1x,c2x) => ax) -> ax
  [w] Deferrable c1x
  [w] Deferrable c2x
-}
