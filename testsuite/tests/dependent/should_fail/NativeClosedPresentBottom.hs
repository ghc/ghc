{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedPresentBottom where
import qualified Data.Maybe
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedChoiceLib as P

bad :: Data.Maybe.isNothing (P.badJust 0) :~: 'False
bad = Refl
