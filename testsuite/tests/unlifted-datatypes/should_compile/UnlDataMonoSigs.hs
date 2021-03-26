{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module UnlDataMonoSigs where

import GHC.Exts
import GHC.Types

data T1 a :: UnliftedType where
  MkT1 :: T1 a

type T2 :: Type -> UnliftedType
data T2 a = T2

type T3 :: Type -> UnliftedType
data T3 a where
  MkT3 :: T3 a

type T4 :: Type -> UnliftedType
data T4 a :: UnliftedType where
  MkT4 :: T4 a
