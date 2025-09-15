{-# LANGUAGE StandaloneKindSignatures #-}

module SAKS_Fail010 where

import Data.Kind (Type)

type T :: Type -> Type
data T = MkT Int
