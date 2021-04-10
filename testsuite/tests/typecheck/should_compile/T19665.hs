{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies,
             RoleAnnotations, FlexibleContexts, AllowAmbiguousTypes #-}

-- See Note [Deriveds do rewrite Deriveds] in GHC.Tc.Types.Constraint
-- for commentary.

module T19665 where

import Data.Coerce

data T a
type role T nominal

type family F a

g :: forall b a. (F a ~ T a, Coercible (F a) (T b)) => ()
g = ()

f :: forall a. (F a ~ T a) => ()
f = g @a
