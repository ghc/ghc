{-# LANGUAGE DataKinds, RequiredTypeArguments, ScopedTypeVariables #-}

module T27588d where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T0 :: forall somename. somename -> Type
type T0 d = Proxy d

data D0 :: T0 d -> Type where
  C0 :: (forall (v :: Type) -> ()) -> D0 f

-- #27588, pattern-signature analogue: the pattern signature (l :: D0 x) does
-- not leak 'somename', so 'r somename' uses only the term binding.
patfun = \ (l :: D0 x) -> C0 $ \somename -> case l of C0 r -> r somename
