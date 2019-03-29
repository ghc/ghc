{-# LANGUAGE TopLevelKindSignatures #-}

module TLKS_001 where

import Data.Kind (Type)

type MonoTagged :: Type -> Type -> Type
data MonoTagged t x = MonoTagged x
