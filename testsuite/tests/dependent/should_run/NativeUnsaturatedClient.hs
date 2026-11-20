{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeUnsaturatedClient where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, U, Multiplicity(Many))
import NativeUnsaturatedProvider (Callback, Same)

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
