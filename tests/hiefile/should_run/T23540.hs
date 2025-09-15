{-# LANGUAGE OverloadedLists, TypeFamilies, ApplicativeDo, NPlusKPatterns #-}

module Main where

import TestUtils
import GHC.IsList ( IsList(..) )

data Modulo1 = Zero deriving (Eq, Ord, Enum)

instance Num Modulo1 where
  fromInteger _ = Zero
  (+) _ _ = Zero

zero :: Modulo1
zero = 0
    -- ^ 1

data Identity a = Identity a

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)
instance Monad Identity where
  Identity x >>= f = f x

foo :: Identity Integer
foo = do
  _x <- Identity 1
    -- ^ 2
  Identity 2

data BetterList x = Nil | Cons x (BetterList x)

instance IsList (BetterList x) where
  type Item (BetterList x) = x
  fromList = foldr Cons Nil
  toList Nil = []
  toList (Cons x xs) = x : toList xs

list :: BetterList Modulo1
list = [0, 1, 2, 3, Zero]
    -- ^ 3    ^ 4   ^ 5

data Letter = A | B | C deriving Enum

letters :: [Letter]
letters = [A .. C]
          -- ^ 6

data Identity' a = Identity' a

instance Functor Identity' where
  fmap f (Identity' x) = Identity' (f x)
instance Applicative Identity' where
  pure = Identity'
  Identity' f <*> Identity' x = Identity' (f x)

bar :: Identity' Integer
bar = do
   -- ^ 7
  a <- Identity' 1
  b <- Identity' 2
  pure (a + b)

isZero :: Modulo1 -> Bool
isZero n = case n of
   0 -> True
-- ^ 8
   _ -> False

instance Real Modulo1 where
  toRational _ = 0

instance Integral Modulo1 where
  toInteger _ = 0
  quotRem _ _ = (0, 0)

isPlusOne :: Modulo1 -> Bool
isPlusOne n = case n of
  (a + 1) -> True
  -- ^ 9
  _ -> False

point1, point2, point3, point4, point5, point6, point7, point8, point9 :: (Int, Int)
point1 = (15, 8)

point2 = (30, 8)

point3 = (43, 8)

point4 = (43, 15)

point5 = (43, 21)

point6 = (49, 14)

point7 = (61, 7)

point8 = (69, 4)

point9 = (82, 6)

points :: [(Int, Int)]
points = [point1, point2, point3, point4, point5, point6, point7, point8, point9]

main = do
  (df, hf) <- readTestHie "T23540.hie"
  let refmap = generateReferencesMap . getAsts $ hie_asts hf
  mapM_ (explainEv df hf refmap) points
