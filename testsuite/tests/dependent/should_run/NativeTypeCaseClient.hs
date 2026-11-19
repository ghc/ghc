{-# LANGUAGE GADTs, DataKinds, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
module NativeTypeCaseClient where

import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy)
import NativeCase

left :: F 'True :~: Int
left = Refl

right :: F 'False :~: Bool
right = Refl

field :: Field ('Just 'True) :~: '( 'True, 'True)
field = Refl

fixed :: Same 'False :~: Int
fixed = Refl

capture :: Capture Int ('Just 'True) :~: '(Int, 'True)
capture = Refl

given :: (a ~ 'True) => Proxy a -> Same a :~: Int
given _ = Refl

brace :: Braces 'False :~: Bool
brace = Refl

fixedKind :: Fixed Ordering 'LT :~: (Proxy 'LT, Proxy 'True)
fixedKind = Refl

captured :: Proxy a -> Captured a 'False :~: Maybe (PayloadKind a)
captured _ = Refl

retained :: Retain Ordering 'LT :~: Proxy 'LT
retained = Refl

shadow :: PatternShadow 'True :~: (Proxy 'False, Maybe Ordering)
shadow = Refl

scrutineeShadow :: ScrutineeShadow 'True :~: Int
scrutineeShadow = Refl

nestedScope :: NestedScope :~: Proxy 'False
nestedScope = Refl

paired :: Paired Ordering 'LT Bool 'True 'True :~: Proxy '( 'LT, 'True)
paired = Refl

inferredCapture :: InferredCapture 'LT 'True :~: Proxy 'LT
inferredCapture = Refl

contextKind :: ContextKind 'True :~: ContextKind 'False
contextKind = Refl

separateKinds :: SeparateKinds 'False :~: Bool
separateKinds = Refl
