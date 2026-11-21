{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module OrdinaryFamilyBranches (example) where

import CheckedEitherInput (Input)
import Data.Type.Equality ((:~:)(Refl))

checked :: Input 'True :~: 'Right ('Just 'False)
checked = Refl

literal :: Input 'False :~: 'Right ('Just 'False)
literal = Refl

example :: String
example = show (checked, literal)
