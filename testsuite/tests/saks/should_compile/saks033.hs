{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, DataKinds, MultiParamTypeClasses, TypeFamilies, TypeApplications #-}

module SAKS_033 where

import Data.Kind
import Data.Proxy

type C :: i -> Constraint
class C (a :: zzz) where
  type F (a :: zzz) :: Type

type T = F @Bool True
