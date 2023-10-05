{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_pat where

import Data.Kind (Type)

f :: forall (a :: Type) -> ()
f (type Int) = ()