{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module UnlDataFams where

import GHC.Exts
import GHC.Types

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

type Interpret :: Bool -> Levity
type family Interpret b where
  Interpret True  = Lifted
  Interpret False = Unlifted

type A :: TYPE (BoxedRep (Interpret b))
data A = MkA Int

a :: A @True
a = MkA 42

-- type Interpret :: Bool -> RuntimeRep
-- type family Interpret b where
--   Interpret True  = BoxedRep Lifted
--   Interpret False = BoxedRep Unlifted
--
-- type A :: TYPE (Interpret b)
-- data A = MkA Int
--
-- data B :: TYPE (Interpret b) where
--   MkB :: Int -> B @b
