{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, PolyKinds, ConstraintKinds #-}

module SAKS_006 where

import Data.Kind (Type, Constraint)

type C :: (k -> Type) -> k -> Constraint
type T :: k -> Type

class C a b
data T a

-- type D :: j -> Constraint -- #16571
type D :: Type -> Constraint
type D = C T

-- type DF :: j -> Constraint -- #16571
type DF :: Type -> Constraint
type family DF where
  DF = C T
