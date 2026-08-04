{-# LANGUAGE DataKinds, RequiredTypeArguments #-}

module T27588a where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T0 :: forall somename. somename -> Type
type T0 d = Proxy d

data D0 :: T0 d -> Type where
  C0 :: (forall (v :: Type) -> ()) -> D0 f

-- #27588: 'somename', a kind-generalised variable from T0's kind, used to leak
-- into tcl_rdr and cause a spurious punned-variable error here.
f :: D0 a -> D0 b
f l = C0 $ \somename -> case l of C0 r -> r somename
