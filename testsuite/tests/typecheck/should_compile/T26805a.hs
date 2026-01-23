{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RoleAnnotations #-}

-- This is a cut-down version of the failure found in !15389
-- when compiling the `constraints` package.
-- We got a Lint error because the NeededEvIds stuff in the
-- constraint solver forgot some needed variables.

module T26805a where

import GHC.Exts (Constraint)
import Data.Kind

data Dict :: Constraint -> Type where
  Dict :: a => Dict a

newtype a :- b = Sub (a => Dict b)
type role (:-) nominal nominal

-- | Instantiate a quantified @'ForallF' p f@ constraint at type @a@.
instF :: forall p f a . ForallF p f :- p (f a)
instF = Sub @(ForallF p f) @(p (f a))
            (case inst :: Forall (ComposeC p f) :- ComposeC p f a of
                Sub Dict -> Dict)

class Forall (ComposeC p f) => ForallF (p :: k2 -> Constraint) (f :: k1 -> k2)

class p (f a) => ComposeC (p :: k2 -> Constraint) (f :: k1 -> k2) (a :: k1)

class (forall a. p a) => Forall (p :: k -> Constraint)
instance (forall a. p a) => Forall (p :: k -> Constraint)

inst :: forall p a. Forall p :- p a
inst = Sub Dict

