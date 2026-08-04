{-# LANGUAGE DataKinds, RequiredTypeArguments #-}

module T27588b where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T0 :: forall somename. somename -> Type
type T0 d = Proxy d

data D0 :: T0 d -> Type where
  C0 :: (forall (v :: Type) -> ()) -> D0 f

-- #27588, class-method analogue of T27588a: 'somename' leaks in via the class
-- variable's kind (Class.hs tcExtendTyVarEnv clas_tyvars).
class C a where
  meth :: D0 a -> D0 b
  meth l = C0 $ \somename -> case l of C0 r -> r somename
