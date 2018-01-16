{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module T11067 where

import Data.Monoid
import GHC.Exts (Constraint)

type family Skolem (p :: k -> Constraint) :: k
type family SkolemF (p :: k2 -> Constraint) (f :: k1 -> k2) :: k1

-- | A quantified constraint
type Forall (p :: k -> Constraint) = p (Skolem p)
type ForallF (p :: k2 -> Constraint) (f :: k1 -> k2) = p (f (SkolemF p f))

-- These work
class ForallF Monoid t => Monoid1 t
instance ForallF Monoid t => Monoid1 t

class ForallF Monoid1 t => Monoid2 t
instance ForallF Monoid1 t => Monoid2 t
-- In both declarations (Forall Monoid1 t) expands to
-- (Monoid1 (t (SkolemF Monoid1 t))), which is simplifiable

-- Changing f a ~ g a to, (Ord (f a), Ord (g a)), say, removes the error
class (f a ~ g a) => H f g a
instance (f a ~ g a) => H f g a

-- This one gives a superclass cycle error.
class Forall (H f g) => H1 f g
instance Forall (H f g) => H1 f g
-- In both declarations (Forall (H f g)) expands to
-- H f g (Skolem (H f g)), which is simplifiable
