{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, Modifiers, TypeOperators #-}
module NativeNamedOpaqueCharacterClient where

import CharacterDecision (Decision, Space, spaceDecision)
import qualified Data.Char as Char
import Data.Type.Equality ((:~:) (Refl))
import GHC.Matchability (U)

atX :: Decision Char.isSpace (Space 'x')
atX = spaceDecision 'x'

ascribed :: Space 'x' :~: (Char.isSpace :: Char %U -> Bool) 'x'
ascribed = Refl

computed :: Char.isSpace 'x' :~: 'False
computed = Refl

#ifdef WRONG_CHARACTER_RESULT
wrong :: Space 'x' :~: 'False
wrong = Refl
#endif

#ifdef MIXED_CHARACTER_CALL
mixed :: Decision Char.isSpace (Char.isSpace 'x')
mixed = spaceDecision 'x'
#endif
