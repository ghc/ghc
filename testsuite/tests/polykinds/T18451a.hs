{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Bug where

import Data.Kind
import Data.Proxy

type Const a b = a

foo :: forall a b (c :: Const Type b). Proxy '[a, c]
foo = error "ruk"

-- We infer a :: k0,  k0 ~ Const Type b
-- We unify k0 := Const Type b (in the eager unifier)
-- And that leaves us with
--    forall (a :: Const Type b) (b :: Type) (c :: Const Type b). ...a
-- Bad!  But delicate because we could expand the synonym
