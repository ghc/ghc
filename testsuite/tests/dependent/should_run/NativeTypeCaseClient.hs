{-# LANGUAGE GADTs, DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
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
