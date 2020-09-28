{-# LANGUAGE GADTSyntax, DataKinds, LinearTypes, KindSignatures, ExplicitForAll #-}
module MultConstructor where

import GHC.Types

data T p a where
  MkT :: a %p -> T p a

{-
this currently fails
g :: forall (b :: Type). T 'Many b %1 -> (b,b)
g (MkT x) = (x,x)
-}
