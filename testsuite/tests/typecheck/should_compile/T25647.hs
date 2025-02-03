{-# LANGUAGE DataKinds, UnliftedNewtypes, TypeFamilies, PolyKinds, MagicHash #-}

module T25647 where

import GHC.Exts
import Data.Kind

-------------------- Plain newtypes -----------------

-- A plain newtype, H98
-- Defaulting happens; infers Fix1 :: forall k. (k -> Type) -> Type
newtype Fix1a f = In1a (f (Fix1a f))

-- A plain newtype, GADT syntax
-- Defaulting happens; infers Fix1 :: forall k. (k -> Type) -> Type
newtype Fix1b f where
    In1b :: forall ff. ff (Fix1b ff) -> Fix1b ff

-- A plain newtype, GADT syntax, with a return kind signature
-- Should infer Fix2 :: forall r k. (k -> TYPE r) -> TYPE r
newtype Fix2 f :: TYPE r where
   In2 :: forall ff. ff (Fix2 ff) -> Fix2 ff

-- Plain newtype, H98 syntax, standalone kind signature
-- Should get In3 :: forall r k (f :: k -> TYPE r). Fix4 @r @k f -> Fix4 @r @k f
--type Fix3 :: forall r k. (k -> TYPE r) -> k
--newtype Fix3 f = In3 (f (Fix3 f))

{-
-- Plain newtype, H98 syntax, standalone kind signature
-- Should get In4 :: forall r k (f :: k -> TYPE r). Fix4 @r @k f -> Fix4 @r @k f
type Fix4 :: forall r k. (k -> TYPE r) -> k
newtype Fix4 f where
  In4 :: forall rr kk (ff :: kk -> TYPE rr).
         ff (Fix4 ff) -> Fix4 @rr @kk ff
-}
-------------------- Data families with newtype instance -----------------

{-
-- data instance in GADT sytntax
data family Dix1 :: (k -> Type) -> k
data instance Dix1 f where
  DIn1 :: forall ff. ff (Dix1 ff) -> Dix1 ff

-- newtype instance in GADT syntax
data family Dix2 :: (k -> Type) -> k
newtype instance Dix2 f where
  DIn2 :: forall ff. ff (Dix2 ff) -> Dix2 ff

-- newtype instance in H98 syntax
data family Dix3 :: (k -> Type) -> k
newtype instance Dix3 f = DIn3 (f (Dix3 f))

-- newtype instance in GADT syntax
data family Dix4 :: (k -> TYPE r) -> k
newtype instance Dix4 f where
  DIn4 :: forall ff. ff (Dix4 ff) -> Dix4 ff

-- newtype instance in H98 syntax
data family Dix5 :: (k -> TYPE r) -> k
newtype instance Dix5 f = DIn5 (f (Dix5 f))

-}
