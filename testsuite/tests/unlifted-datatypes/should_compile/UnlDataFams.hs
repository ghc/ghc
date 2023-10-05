{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module UnlDataFams where

import Data.Kind
import GHC.Exts

data family F a :: UnliftedType

data instance F Int = TInt

data family G a :: TYPE (BoxedRep l)

data instance G Int :: Type where
  GInt :: G Int
data instance G Bool :: UnliftedType where
  GBool :: G Bool
data instance G Char :: Type where
  GChar :: G Char

data family H :: Type -> UnliftedType
data instance H Int = HInt Int
