{-# language TypeFamilies, DataKinds, UnboxedTuples #-}

module ListTuplePunsFail4 where

import Data.Kind (Type)
import GHC.Exts (TYPE)

type family F2 (a :: Type -> Type -> TYPE rep) :: Type where

type T3 = F2 (#,#)
