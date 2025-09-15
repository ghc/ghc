{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies,
             RoleAnnotations, FlexibleContexts, AllowAmbiguousTypes #-}

-- See #19665 for commentary.

module T19665 where

import Data.Coerce

data T a
type role T nominal

type family F a

g :: forall b a. (F a ~ T a, Coercible (F a) (T b)) => ()
g = ()

f :: forall a. (F a ~ T a) => ()
f = g @a
