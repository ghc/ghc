{-# OPTIONS_GHC -Wmissing-role-annotations #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module T22702a where

import Data.Kind (Type)

-- type with parameters
-- warns
type Foo :: Type -> Type -> Type
data Foo x y = Foo x

-- type without parameters
-- doesn't warn
data Quu = Quu1 | Quu2

-- polykinded type
-- warns, no role for `k`
type Bar :: (k -> Type) -> k -> Type
data Bar f a = Bar (f a)

-- type-class may have roles as well
-- doesn't warn
class C a where
