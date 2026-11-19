{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, StandaloneKindSignatures,
             TypeFamilies, TypeOperators #-}
-- | Apply a type lambda to a list of type constructors.
module TypeLambda
  ( L, Captured, Shadowed, WithError, Mixed, Held, CaseShadow, Rebuild
  , roadmap, partial, mixed
  ) where

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

-- | Supply the error type now and the result type at the use site.
type WithError :: Type %U -> Type
type WithError = (\err result -> Either err result) String

-- | Capture the first argument of Either from the enclosing synonym.
type Captured (a :: Type) = Map (\b -> Either a b) '[Bool, Char]

-- | Keep the outer kind of a while the lambda binds its own q.
type Shadowed (q :: Type) (a :: q) = Map (\q -> Either q (Proxy a)) '[Char]

-- | Check the full result of the roadmap example.
roadmap :: L :~: '[Maybe Int, Either String Int, IO Int]
roadmap = Refl

-- | Apply the partially applied lambda to Bool.
partial :: WithError Bool :~: Either String Bool
partial = Refl

-- | Match each element with a tail case on the lambda parameter.
type Mixed = Map (\b -> case b of { True -> Int; False -> Bool }) '[ 'True, 'False]

-- | A rigid scrutinee leaves the constructor match stuck.
type Held :: Bool -> Type
type Held b = (\tag -> case tag of { True -> Int; False -> Int }) b

-- | The Just field shadows a in that branch. Nothing uses the outer a.
type CaseShadow (a :: k) =
  Map (\b -> case b of { Just a -> Proxy a; Nothing -> Proxy a }) '[ 'Just 'True, 'Nothing]

-- | Check both alternatives in the mapped result.
mixed :: Mixed :~: '[Int, Bool]
mixed = Refl

-- | Preserve the entire scrutinee after matching its constructor.
type Rebuild :: forall k. Maybe k -> Type
type Rebuild m = (\tag -> case tag of { Nothing -> Proxy tag; Just _ -> Proxy tag }) m
