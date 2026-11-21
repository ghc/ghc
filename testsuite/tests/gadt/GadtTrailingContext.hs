{-# LANGUAGE ExplicitNamespaces, GADTs, ImpredicativeTypes, LinearTypes, RequiredTypeArguments, TypeAbstractions, TypeApplications, TypeOperators, ViewPatterns #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import GadtTrailingContextProvider

observe :: T a -> a -> a -> (Int, Bool)
observe (MkT n @a) x y = (n, x == y)
observe (Force n @a) x y = (n, x == y)

refined :: Checked a b -> (a :~: Int, b :~: Bool)
refined (Checked _ @b) = (Refl, Refl)

constructorAlias :: Int %1 -> forall a. Eq a => T a
constructorAlias = MkT

newtype Badge = Badge Int
instance Eq Badge where
  Badge a == Badge b = odd a == odd b

data Rank = Rank Int deriving Eq
instance Ord Rank where
  compare (Rank a) (Rank b) = compare b a

compareFields :: Combined a b -> a -> a -> b -> b -> (Ordering, Bool)
compareFields (Combined Evidence) x y u v = (compare x y, u == v)

compareInPattern :: Combined a b -> a -> a -> Bool
compareInPattern (Combined Evidence) x (compare x -> LT) = True
compareInPattern (Combined _) _ _ = False

data Never = Never
instance Eq Never where
  _ == _ = False

existsEq :: F -> Bool
existsEq (K x) = x == x

indexedEq :: Box a -> a -> Bool
indexedEq (Box x) y = x == y

standaloneAlias :: forall a. a %1 -> Eq a => F
standaloneAlias = K

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> try @SomeException (evaluate x)

main :: IO ()
main = do
  print (observe (MkT 29 @Badge) (Badge 1) (Badge 3))
  print (observe (constructorAlias 31 @Badge) (Badge 1) (Badge 2))
  let bottom = error "strict prefix" :: Int
  partial <- throws (seq @(forall a. Eq a => T a) (Force bottom) True)
  strict <- throws (Force bottom @Badge)
  lazy <- throws (MkT bottom @Badge)
  print (partial, strict, lazy)
  print (case refined (Checked @Int (Witness (type Int)) @Bool) of (Refl, Refl) -> True)
  let combined = Combined @Rank Evidence @Badge
      missing = Combined @Rank (error "nested matcher") @Badge
  print (compareFields combined (Rank 1) (Rank 2) (Badge 1) (Badge 3))
  print (compareInPattern combined (Rank 2) (Rank 1),
         compareInPattern combined (Rank 1) (Rank 2))
  print =<< throws (compareInPattern missing (Rank 2) (Rank 1))
  print (existsEq (standaloneAlias Never), existsEq (K True),
         indexedEq (Box (Badge 1)) (Badge 3))
