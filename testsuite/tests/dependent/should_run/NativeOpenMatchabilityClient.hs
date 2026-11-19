{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module NativeOpenMatchabilityClient where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import T14038 (AppType(App), FunArrow(..), FunType(Fun))

ordinary :: Fun Type '(:->) Type
ordinary x = x

higherKind :: App Type '(:->) Type Maybe Bool :~: Maybe Bool
higherKind = Refl
