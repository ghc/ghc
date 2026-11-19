{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedIndexedWrongValue where
import Data.Type.Equality ((:~:)(Refl))
import qualified IndexedMark as P

wrong :: P.sample 6 :~: 'P.Pair 7 6
wrong = Refl
