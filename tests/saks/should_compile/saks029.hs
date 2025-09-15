{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, DataKinds, RankNTypes, TypeFamilies #-}

module SAKS_029 where

import Data.Kind
import Data.Proxy
import Data.Type.Bool

type IfK :: forall j m n. forall (e :: Proxy (j :: Bool)) -> m -> n -> If j m n
type family IfK e f g where
   IfK (_ :: Proxy True)  f _ = f
   IfK (_ :: Proxy False) _ g = g
