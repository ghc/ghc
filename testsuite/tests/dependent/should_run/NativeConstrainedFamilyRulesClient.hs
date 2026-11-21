{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications, TypeFamilies, TypeOperators #-}
module Main where
import NativeConstrainedFamilyRulesProvider
import FinitePosition (FinView(AtZero))
import PackedVecLibrary (Nat(Z))
import Data.Type.Equality
good :: Elem [Bool] :~: Bool
good = Refl
main :: IO ()
main = case good of Refl -> print (case zero @'Z of AtZero -> (consume True (make True), consume False (make False)))
