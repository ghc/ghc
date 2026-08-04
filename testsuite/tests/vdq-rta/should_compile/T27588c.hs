{-# LANGUAGE DataKinds, RequiredTypeArguments, FlexibleInstances #-}

module T27588c where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T0 :: forall somename. somename -> Type
type T0 d = Proxy d

data D0 :: T0 d -> Type where
  C0 :: (forall (v :: Type) -> ()) -> D0 f

class C a where
  meth :: D0 a -> D0 b

-- #27588, instance-method analogue: 'somename' leaks in via the instance-head
-- variable's kind (Instance.hs ib_tyvars).
instance C (x :: T0 d) where
  meth l = C0 $ \somename -> case l of C0 r -> r somename
