{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedIndexedWrongKind where
import Data.Type.Equality ((:~:)(Refl))
import qualified IndexedMark as P

wrong :: P.sample 0 :~: 'P.Off
wrong = Refl
