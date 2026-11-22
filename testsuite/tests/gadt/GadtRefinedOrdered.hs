{-# LANGUAGE ExplicitNamespaces, GADTs, RankNTypes, RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeAbstractions, TypeFamilies, TypeOperators #-}
module Main where

import Data.Proxy
import Data.Type.Equality
import GadtRefinedOrderedProvider
import qualified Language.Haskell.TH as TH

same :: $(TH.reifyType 'Same)
same = Same

isReflexive :: Refined x y -> Bool
isReflexive (Same x (type _) _) = x == x
isReflexive (List x (type _) _) = x == x
isReflexive (Fixed x (type _) _) = x == x

resultMatches :: Refined x y -> Either (x :~: y) (Either (x :~: [y]) (x :~: Int, y :~: Bool))
resultMatches (Same _ (type _) _) = Left Refl
resultMatches (List _ (type _) _) = Right (Left Refl)
resultMatches (Fixed _ (type _) _) = Right (Right (Refl, Refl))

withResult :: TrailingType x -> (forall b. x :~: [b] -> r) -> r
withResult (TrailingType _ (type b)) use = use @b Refl

useResultEquality :: forall x y. TrailingContext x y -> Bool
useResultEquality (TrailingContext (_ :: Proxy y)) = True

family :: $(TH.reifyType 'Family)
family = Family

familyProof :: Family Char x y -> (x :~: [y], Bool)
familyProof (Family value (type _) _) = (Refl, value == value)

main :: IO ()
main = do
  let s x = same @Double x (type Bool) Proxy
      l x = List @Double x (type Bool) Proxy
      f x = Fixed @Double x (type Bool) Proxy
      observe make = (isReflexive (make 3), isReflexive (make (0 / 0)), resultMatches (make 3))
  print (observe s, observe l, observe f)
  print (withResult (TrailingType (3 :: Double) (type Char)) (\Refl -> True),
         useResultEquality (TrailingContext @Double Proxy))
  let example value = family @Double value (type Bool) Proxy
  print (familyProof (example 3), familyProof (example (0 / 0)))
