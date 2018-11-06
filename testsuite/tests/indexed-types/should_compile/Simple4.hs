{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Data.Kind (Type)

class C8 a where
  data S8 a :: Type -> Type

instance C8 Int where
  data S8 Int a = S8Int a
