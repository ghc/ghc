{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NativeTypeCaseLetWrong where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeCase (Paired)

wrong :: Paired Ordering 'LT Bool 'True 'True :~: Proxy '( 'GT, 'True)
wrong = Refl
