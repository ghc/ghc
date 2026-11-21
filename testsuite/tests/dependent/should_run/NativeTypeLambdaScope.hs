{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module NativeTypeLambdaScope
  ( Levels, Adjacent, InferredLevels, UsesKind, LetKind
  , Selected, Siblings, OrdinaryCases, Consed, SelectedFirst
  ) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Exts (U)

type ApplyTwo :: (Type %U -> Type %U -> Type) -> Type -> Type -> Type
type family ApplyTwo f a b where ApplyTwo f a b = f a b

type Pair x y = (x,y)

type Levels (q :: Type) (a :: q) =
  (\q -> Pair (Proxy a, q)
         (ApplyTwo (\q b -> (Proxy a, q, b)) Int Ordering)) Bool

type LetKind (k :: Type) (x :: k) =
  let k = Maybe k in (\a -> (k, Proxy x, a)) Char

type Adjacent (q :: Type) (a :: q) =
  ApplyTwo (\q -> \q -> (Proxy a, q)) Int Ordering

type InferredLevels (a :: q) =
  (\q -> Pair (Proxy a, q)
         (ApplyTwo (\q b -> (Proxy a, q, b)) Int Ordering)) Bool

type UsesKind (a :: k) =
  (\b c -> (Proxy (a :: k), Proxy k, b, c)) Bool Char

type Selected xs = '[ (\b -> case b of { True -> Bool; False -> Ordering }) xs ]

type Siblings b =
  '( (\x -> case x of { True -> Bool; False -> Ordering }) b
   , (\x -> case x of { True -> Char; False -> Int }) b )

type OrdinaryCases (b :: Bool) =
  ( [case b of { True -> Bool; False -> Ordering }]
  , Maybe (case b of { True -> Char; False -> Int }) )

type Consed (b :: Bool) =
  (case b of { True -> Char; False -> Ordering }) ': '[Int]


type SelectedFirst b t = Maybe ((\tag x ->
  case tag of { True -> Either x Bool; False -> Either Bool x }) b t)
