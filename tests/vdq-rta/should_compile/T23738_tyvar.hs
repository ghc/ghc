{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T23738_tyvar where

import Data.Kind (Type)

checkEq :: forall a b -> (a ~ b) => ()
checkEq (type _) (type _) = ()

result :: forall k. forall (f :: k -> Type) (t :: k) -> ()
result (type f) (type t) =
  checkEq
  {- type syntax: -} (type (f t))
  {- term syntax: -}       (f t)