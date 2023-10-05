{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_callStack where

import Data.Kind (Type)
import GHC.Stack

f :: forall (a :: Type) -> CallStack
f (type _) = callStack