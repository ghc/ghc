{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module T26020 where

import Data.Kind
import Data.Coerce

class C r where
  data T r :: Type -> Type

-- type family F r
-- data T r i = MkR r (F i)

coT :: (forall x. Coercible (T r i) (T r o))
    => T r i -> T r o
coT = coerce
