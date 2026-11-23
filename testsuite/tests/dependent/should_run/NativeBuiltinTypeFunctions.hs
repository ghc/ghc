{-# LANGUAGE CPP, DataKinds, ExplicitNamespaces, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeOperators #-}
module NativeBuiltinTypeFunctions where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import GHC.TypeLits (AppendSymbol, Symbol)
import GHC.TypeNats (CmpNat, Nat, type (+))

type Map :: (a %U -> b) -> [a] -> [b]
type family Map f xs where
    Map _ '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

type Prefixed :: [Symbol]
#ifdef WRAPPED
type AddOne (n :: Nat) = 1 + n
type CompareThree (n :: Nat) = CmpNat 3 n
type Prefix (s :: Symbol) = AppendSymbol "ghc-" s

type Added = Map AddOne '[2, 3, 4]
type Compared = Map CompareThree '[2, 3, 4]
type Prefixed = Map Prefix '["one", "two"]
#else
type Added = Map ((+) 1) '[2, 3, 4]
type Compared = Map (CmpNat 3) '[2, 3, 4]
type Prefixed = Map (AppendSymbol "ghc-") '["one", "two"]
#endif

added :: Added :~: '[3, 4, 5]
added = Refl

compared :: Compared :~: '[ 'GT, 'EQ, 'LT ]
compared = Refl

prefixed :: Prefixed :~: '["ghc-one", "ghc-two"]
prefixed = Refl

unknown :: forall (n :: Nat). Map ((+) 1) '[n] :~: '[1 + n]
unknown = Refl
