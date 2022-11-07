{-# LANGUAGE TypeData, DataKinds, TypeFamilies #-}
module Main where

import Type.Reflection
import Data.Type.Equality

data Proxy a
type data X1 = T -- defines type constructor T
data X2 = T      -- defines type constructor 'T

data family F p

newtype instance F (Proxy T) = ID (forall a. a -> a)
newtype instance F (Proxy 'T) = UC (forall a b. a -> b)

-- This should fail at runtime because these are different types
eq :: T :~~: 'T
Just eq = eqTypeRep typeRep typeRep

p :: a :~~: b -> F (Proxy a) :~: F (Proxy b)
p HRefl = Refl

uc :: a -> b
uc = case castWith (p eq) (ID id) of UC a -> a

main :: IO ()
main = print (uc 'a' :: Int)
