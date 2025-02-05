{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, MagicHash #-}

module T25647b where

import GHC.Exts
import Data.Kind

---------------------------
-- without UnliftedNewtypes
---------------------------

-------------------- Plain newtypes -----------------

-- A plain newtype, H98
-- Defaulting happens; infers Fix1 :: forall k. (k -> Type) -> Type
newtype Fix1a f = In1a (f (Fix1a f))

-- A plain newtype, GADT syntax
-- Defaulting happens; infers Fix1 :: forall k. (k -> Type) -> Type
newtype Fix1b f where
    In1b :: forall ff. ff (Fix1b ff) -> Fix1b ff


-------------------- Data families with newtype instance -----------------

-- data instance in GADT sytntax
data family Dix1 :: (k -> Type) -> k
data instance Dix1 f where
  DIn1 :: forall ff. ff (Dix1 ff) -> Dix1 ff

-- newtype instance in GADT syntax
data family Dix2 :: (k -> Type) -> k
newtype instance Dix2 f where
  DIn2 :: forall ff. ff (Dix2 ff) -> Dix2 ff

data family Dix2a :: (k -> Type) -> k
newtype instance Dix2a f :: Type where
  DIn2a :: forall ff. ff (Dix2a ff) -> Dix2a ff

-- newtype instance in H98 syntax
data family Dix3 :: (k -> Type) -> k
newtype instance Dix3 f = DIn3 (f (Dix3 f))

-- newtype instance in H98 syntax
data family Dix5 :: (k -> TYPE r) -> k
newtype instance Dix5 f = DIn5 (f (Dix5 f))

-- data family Dix6 :: (k -> TYPE 'IntRep) -> k
-- newtype instance Dix6 f where
--   DIn6 :: forall ff. ff (Dix6 ff) -> Dix6 ff
