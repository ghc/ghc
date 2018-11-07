{-# LANGUAGE PolyKinds, MultiParamTypeClasses, GADTs, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Super where

import Data.Kind
import Data.Proxy
import GHC.Prim

class (a ~ b) => C a b
data SameKind :: k -> k -> Type where
  SK :: SameKind a b

bar :: forall (a :: Type) (b :: Type). C a b => Proxy a -> Proxy b -> ()
bar _ _ = const () (undefined :: forall (x :: a) (y :: b). SameKind x y)
