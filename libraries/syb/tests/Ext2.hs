{-# LANGUAGE DeriveDataTypeable #-}

module Ext2 (tests) where

-- Tests for ext2 and friends

import Test.Tasty.HUnit
import Data.Generics


-- A type of lists
data List a = Nil | Cons a (List a) deriving (Data, Typeable, Show, Eq)

-- Example lists
l1, l2 :: List Int
l1 = Cons 1 (Cons 2 Nil)
l2 = Cons 0 l1

-- A type of pairs
data Pair a b = Pair1 a b | Pair2 a b deriving (Data, Typeable, Show, Eq)

-- Example pairs
p1, p2 :: Pair Int Char
p1 = Pair1 2 'p'
p2 = Pair2 3 'q'

-- Structures containing the above
s1 :: [Pair Int Char]
s1 = [p1, p2]

s2 :: (Pair Int Char, List Int)
s2 = (p2, l2)


-- Auxiliary functions
unifyPair :: Pair a b -> Pair a b -> Bool
unifyPair (Pair1 _ _) (Pair1 _ _) = True
unifyPair (Pair2 _ _) (Pair2 _ _) = True
unifyPair _           _           = False

flipPair :: Pair a b -> Pair a b
flipPair (Pair1 a b) = Pair2 a b
flipPair (Pair2 a b) = Pair1 a b

-- Tests
t1 = everywhere (id `ext2T` flipPair) (s1,s2)
t2 = let f :: (Data a) => a -> Maybe a
         f = (const Nothing) `ext2M` (Just . flipPair)
     in (f p1, f l1)
t3 = everything (+) ( const 0
             `ext1Q` (const 1  :: List a   -> Int)
             `ext2Q` (const 10 :: Pair a b -> Int))
               $ s2
t4 = unifyPair (t4' :: Pair Int Char) t4' where
  t4' :: Data a => a
  t4' = undefined `ext1B` Nil `ext2B` (Pair1 undefined undefined)


-- Main function for testing
tests = (t1, t2, t3, t4) @=? output

output = ((map flipPair s1, (flipPair p2, l2))
         ,(Just (flipPair p1),Nothing)
         ,14
         ,True)
