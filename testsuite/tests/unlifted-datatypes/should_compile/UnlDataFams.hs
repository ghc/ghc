{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module UnlDataFams where

import GHC.Exts
import GHC.Types

data family F a :: UnliftedType

data instance F Int = TInt

data family G a :: TYPE (BoxedRep l)

data instance G Int = GInt
data instance G Bool :: UnliftedType where
  GBool :: G Bool
data instance G Char :: Type where
  GChar :: G Char

