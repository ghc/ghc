{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeUnsaturatedClient where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, M, U, Multiplicity(Many))
import NativeUnsaturatedProvider (Callback, Same, ForwardMap, WrittenMap, NamedMap)

type Alias a = Callback a

type Apply :: FUN @U 'Many Bool Bool -> Bool
type Apply f = f 'True

reflexive :: Same (Alias 'True) (Alias 'True) :~: 'True
reflexive = Refl

saturated :: Apply (Alias 'True) :~: 'True
saturated = Refl

given :: f :~: Alias 'True -> Apply f :~: 'True
given Refl = Refl

-- Both prefixes agree after rewriting with the index equality.
congruence :: a :~: b -> Callback a :~: Callback b
congruence Refl = Refl

forwardedFamily :: ForwardMap (Alias 'True) '[ 'True, 'True] :~: '[ 'True, 'True]
forwardedFamily = Refl

forwardedConstructor :: ForwardMap Maybe '[Bool, Int] :~: '[Maybe Bool, Maybe Int]
forwardedConstructor = Refl

writtenConstructor :: WrittenMap Maybe '[Bool, Int] :~: '[Maybe Bool, Maybe Int]
writtenConstructor = Refl

namedFamily :: NamedMap @Bool @U @Bool (Alias 'True) '[ 'True] :~: '[ 'True]
namedFamily = Refl

namedConstructor :: NamedMap @Type @M @Type Maybe '[Bool] :~: '[Maybe Bool]
namedConstructor = Refl
