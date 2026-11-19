{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedCallbackKinds where

import qualified Data.Char as C
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedCallbackLib as W
import qualified Prelude as P

bad :: W.eachBool C.toUpper '[ 'P.True] :~: '[ 'P.False]
bad = Refl
