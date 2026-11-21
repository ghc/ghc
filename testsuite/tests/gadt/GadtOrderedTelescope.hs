{-# LANGUAGE GADTs, Haskell2010, ImpredicativeTypes, NoExperimentalDependentTypes #-}
{-# LANGUAGE ExplicitNamespaces, RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions, TypeApplications, ViewPatterns #-}
module Main where

import Control.Exception
import GadtOrderedTelescopeProvider

equalMixed :: Mixed a -> Bool
equalMixed (Mixed _ @a (x :: a)) = x == x

-- | Use equality when matching the field after the context.
readFieldCheck :: FieldCheck a -> Bool
readFieldCheck (FieldCheck _ ((\x -> x == x) -> result)) = result

equalPair :: Reversed a b -> (Bool, b)
equalPair (Reversed @a _ @b ((a :: a), (b :: b))) = (a == a, b)

readSeveral :: Several a b -> (String, Bool, String)
readSeveral (Several _ x @b (y :: b)) = (show x, x == x, show y)

readLayered :: Layered a -> Int
readLayered (Layered Nothing @b _ (Tag (type Int) n)) = n
readLayered (Layered (Just _) @b _ _) = 0

readRefined :: Refined a -> (Ordering, String)
readRefined (Refined StoredOrder @b (w :: b) False) = (compare w w, show w)
readRefined (Refined StoredOrder @b (w :: b) True) = (compare w w, show w ++ "!")

readF3 :: F3 a -> (Bool, Bool)
readF3 (MkF3 x y) = (x == x, y == y)

readF4 :: F4 a -> (Bool, Bool)
readF4 (MkF4 x y) = (x == x, y == y)

readF5 :: F5 a -> (Int, Int, a, Int, Int)
readF5 (MkF5 i j @a x k l @b _) = (i, j, x, k, l)

readF2 :: F2 a -> (Ordering, Bool)
readF2 (MkF2 x) = (compare x x, x == x)

keepF2 :: F2 a -> Bool
keepF2 (MkF2 _) = True

less :: Ord a => (a, a) -> Bool
less (x, y) = x < y

readFence :: Fenced a -> Int
readFence (Fenced (less -> True)) = 1
readFence (Fenced (x, _)) = x

readLate :: Late a b -> String
readLate (Late x @b) = show x

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> try @SomeException (evaluate x)

main :: IO ()
main = do
  print (equalMixed (Mixed 1 @Bool True), equalMixed (Mixed 2 @Double (0/0)),
         equalPair (Reversed @Bool 3 @Char (True, 'c')),
         readSeveral (Several @Double 4 (0/0) @Char 'q'),
         readLayered (Layered @Int Nothing @Int (error "unused earlier field") (Tag (type Int) 41)),
         readLayered (Layered @Int (Just ()) @Int (error "unused earlier field") (error "unused last field")),
         readRefined (Refined @Int StoredOrder @Int 59 True))
  print (readF3 (MkF3 @Bool True @Double (0/0)),
         readF4 (MkF4 @Double @Bool (0/0) True),
         readF5 (MkF5 1 2 @Bool True 3 4 @Char 'x'))
  print ( readFieldCheck (FieldCheck (error "unused earlier field") True)
        , readFieldCheck (FieldCheck (error "unused earlier field") (0/0 :: Double)) )
  let bad = error "strict prefix" :: Int
  partial <- throws (seq @(forall a. Eq a => a -> Mixed a) (Mixed bad) True)
  typed <- throws (Mixed bad @Bool)
  sat <- throws (Mixed bad @Bool True)
  lazy <- throws (Mixed 5 @Bool (error "lazy suffix"))
  print (partial, typed, sat, lazy)
  print (readF2 (MkF2 @Bool True), readFence (Fenced (2, 3)),
         readFence (Fenced (41, 5)), readLate (Late @Int 59 @Bool))
  let bottom = error "strict field" :: Int
  partialLate <- throws (seq @(forall b. Eq b => Late Int b) (Late @Int bottom) True)
  saturatedLate <- throws (Late @Int bottom @Bool)
  lazyF2 <- throws (keepF2 (MkF2 @Bool (error "unused lazy field")))
  print (partialLate, saturatedLate, lazyF2)
