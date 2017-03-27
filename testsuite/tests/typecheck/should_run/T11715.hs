{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies, TypeOperators #-}

import Data.Kind
import Data.Typeable

type family F x a b
type instance F Type       a b = a
type instance F Constraint a b = b

foo :: x :~: y -> F x a b -> F y a b
foo Refl = id

unsafeCoerce :: a -> b
unsafeCoerce x = case eqT :: Maybe (Type :~: Constraint) of
                   Nothing -> error "No more bug!"
                   Just r  -> foo r x

main :: IO ()
main = let x :: Int
           x = 42
       in print (unsafeCoerce x :: Double)
