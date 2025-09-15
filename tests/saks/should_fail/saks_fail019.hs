{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module SAKS_Fail019 where

import Data.Kind (Type)

type T :: Type -> Type -> Type
data T x :: x -> Type
  -- Should not panic with:
  --   GHC internal error: ‘x’ is not in scope during type checking, but it passed the renamer
