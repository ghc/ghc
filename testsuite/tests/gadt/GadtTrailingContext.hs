{-# LANGUAGE ExplicitNamespaces, GADTs, ImpredicativeTypes, RequiredTypeArguments, TypeAbstractions, TypeApplications, TypeOperators #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import GadtTrailingContextProvider

observe :: T a -> a -> a -> (Int, Bool)
observe (MkT n @a) x y = (n, x == y)
observe (Force n @a) x y = (n, x == y)

refined :: Checked a b -> (a :~: Int, b :~: Bool)
refined (Checked _ @b) = (Refl, Refl)

newtype Badge = Badge Int
instance Eq Badge where
  Badge a == Badge b = odd a == odd b

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> try @SomeException (evaluate x)

main :: IO ()
main = do
  print (observe (MkT 29 @Badge) (Badge 1) (Badge 3))
  let bottom = error "strict prefix" :: Int
  partial <- throws (seq @(forall a. Eq a => T a) (Force bottom) True)
  strict <- throws (Force bottom @Badge)
  lazy <- throws (MkT bottom @Badge)
  print (partial, strict, lazy)
  print (case refined (Checked @Int (Witness (type Int)) @Bool) of (Refl, Refl) -> True)
