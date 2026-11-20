{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeUnsaturatedProvider (Select, Callback, Same, ForwardMap, WrittenMap, NamedMap) where

import GHC.Exts (FUN, U, Multiplicity(Many))
import GHC.Matchability (Matchability)

type family Select (a :: Bool) (b :: Bool) :: Bool
type instance Select a 'True = a

type Callback :: Bool -> FUN @U 'Many Bool Bool
type Callback a = Select a

type Same :: FUN @U 'Many Bool Bool -> FUN @U 'Many Bool Bool -> Bool
type family Same f g where
  Same f f = 'True
  Same f g = 'False

type family Map f xs where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family ForwardMap f xs where
  ForwardMap _ '[] = '[]
  ForwardMap f (x ': xs) = f x ': Map f xs

type WrittenMap :: (a -> b) -> [a] -> [b]
type family WrittenMap f xs where
  WrittenMap _ '[] = '[]
  WrittenMap f (x ': xs) = f x ': WrittenMap f xs

type NamedMap :: forall a (m :: Matchability) b. FUN @m 'Many a b -> [a] -> [b]
type family NamedMap @a @m @b f xs where
  NamedMap @a @m @b _ '[] = '[]
  NamedMap @a @m @b f (x ': xs) = f x ': NamedMap @a @m @b f xs
