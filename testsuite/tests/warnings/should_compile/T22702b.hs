{-# OPTIONS_GHC -Wmissing-role-annotations #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module T22702b where

import Data.Kind (Type)

-- type with parameters
type Foo :: Type -> Type -> Type
type role Foo representational phantom
data Foo x y = Foo x

-- type without parameters
data Quu = Quu1 | Quu2

-- polykinded type
type Bar :: (k -> Type) -> k -> Type
type role Bar representational nominal
data Bar f a = Bar (f a)

-- type-class may have roles as well
class C a where
