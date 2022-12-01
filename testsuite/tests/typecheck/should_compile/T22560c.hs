{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module T22560c where

import Data.Kind
import Data.Proxy

-- Associated type and data families with @-binders
type C :: forall k. k -> Constraint
class C @j (a :: j) where
  type F @j a
  data D @j a
  f :: Proxy a -> F @j a
  g :: Proxy a -> D @j a

-- Instance with an @-binder in F, D
instance C True where
  type F @Bool True = ()
  data D @Bool True = MkTrue
  f _ = ()
  g _ = MkTrue

-- Instance without an @-binder in F, D
instance C False where
  type F False = ()
  data D False = MkFalse
  f _ = ()
  g _ = MkFalse