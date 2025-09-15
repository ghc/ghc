{-# language TypeFamilies, DataKinds #-}

module ListTuplePunsFail3 where

import Data.Kind (Type)
import GHC.Exts (TYPE)

type family F2 (a :: Type -> Type -> TYPE rep) :: Type where

type T2 = F2 (,)
