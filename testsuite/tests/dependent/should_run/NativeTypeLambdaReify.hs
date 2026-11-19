{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell, TypeOperators #-}
module NativeTypeLambdaReify where

import Data.Type.Equality ((:~:)(Refl))
import Language.Haskell.TH
import TypeLambda (L)

$(do info <- reify ''L
     case info of
       TyConI (TySynD _ [] rhs) -> pure [TySynD (mkName "RoundTrip") [] rhs]
       _ -> fail "expected the type synonym")

checked :: RoundTrip :~: '[Maybe Int, Either String Int, IO Int]
checked = Refl
