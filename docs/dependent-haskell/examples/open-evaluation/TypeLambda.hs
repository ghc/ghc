{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, StandaloneKindSignatures,
             TypeFamilies, TypeOperators #-}
-- | Apply a type lambda to a list of type constructors.
module TypeLambda (L, Captured, Shadowed, roadmap) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)

-- | Apply a type function to each list element.
type Map :: (a %U -> b) -> [a] -> [b]
type family Map f xs where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- | Apply each type constructor to Int.
type L = Map (\f -> f Int) [Maybe, Either String, IO]

-- | Capture the first argument of Either from the enclosing synonym.
type Captured (a :: Type) = Map (\b -> Either a b) '[Bool, Char]

-- | Keep the outer kind of a while the lambda binds its own q.
type Shadowed (q :: Type) (a :: q) = Map (\q -> Either q (Proxy a)) '[Char]

-- | Check the full result of the roadmap example.
roadmap :: L :~: '[Maybe Int, Either String Int, IO Int]
roadmap = Refl
