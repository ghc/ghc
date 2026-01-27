
{-# LANGUAGE TypeFamilies #-}

module T25949_aux
  ( Opaque, OutOfScope
  , OpaqueNT
  , OutOfScopeNT(..)
  , FamNT(MkOutOfScopeFamNT)
  )
  where

import Data.Kind

newtype OpaqueNT = MkOpaqueNT Int
newtype OutOfScopeNT = MkOutOfScopeNT Int

type FamNT :: Type -> Type
data family FamNT a

data Opaque
data OutOfScope

newtype instance FamNT Opaque = MkOpaqueFamNT Int
newtype instance FamNT OutOfScope = MkOutOfScopeFamNT Int
