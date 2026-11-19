{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module NativeTypeLambdaScope (Levels, Adjacent, InferredLevels, UsesKind) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Exts (U)

type ApplyTwo :: (Type %U -> Type %U -> Type) -> Type -> Type -> Type
type family ApplyTwo f a b where ApplyTwo f a b = f a b

type Pair x y = (x,y)

type Levels (q :: Type) (a :: q) =
  (\q -> Pair (Proxy a, q)
         (ApplyTwo (\q b -> (Proxy a, q, b)) Int Ordering)) Bool

type Adjacent (q :: Type) (a :: q) =
  ApplyTwo (\q -> (\q -> (Proxy a, q))) Int Ordering

type InferredLevels (a :: q) =
  (\q -> Pair (Proxy a, q)
         (ApplyTwo (\q b -> (Proxy a, q, b)) Int Ordering)) Bool

type UsesKind (a :: k) =
  (\b c -> (Proxy (a :: k), Proxy k, b, c)) Bool Char
