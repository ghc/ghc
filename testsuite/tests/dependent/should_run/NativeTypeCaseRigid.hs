{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NativeTypeCaseRigid where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeCase

blocked :: Proxy a -> Same a :~: Int
blocked _ = Refl

shadow :: Proxy x -> Proxy a -> Capture x a :~: '(x, 'False)
shadow _ _ = Refl
