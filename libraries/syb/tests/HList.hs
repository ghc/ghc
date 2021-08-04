{-# OPTIONS -fglasgow-exts #-}

module HList (tests) where

{-

This module illustrates heterogeneously typed lists.

-}

import Test.Tasty.HUnit

import Data.Typeable


-- Heterogeneously typed lists
type HList = [DontKnow]

data DontKnow = forall a. Typeable a => DontKnow a

-- The empty list
initHList :: HList
initHList = []

-- Add an entry
addHList :: Typeable a => a -> HList -> HList
addHList a l = (DontKnow a:l)

-- Test for an empty list
nullHList :: HList -> Bool
nullHList = null

-- Retrieve head by type case
headHList :: Typeable a => HList -> Maybe a
headHList [] = Nothing
headHList (DontKnow a:_) = cast a

-- Retrieve tail by type case
tailHList :: HList -> HList
tailHList = tail

-- Access per index; starts at 1
nth1HList :: Typeable a => Int -> HList -> Maybe a
nth1HList i l = case (l !! (i-1)) of (DontKnow a) -> cast a


----------------------------------------------------------------------------

-- A demo list
mylist = addHList (1::Int)       $
         addHList (True::Bool)   $
         addHList ("42"::String) $
         initHList

-- Main function for testing
tests = (   show (nth1HList 1 mylist :: Maybe Int)    -- shows Just 1
        , ( show (nth1HList 1 mylist :: Maybe Bool)   -- shows Nothing
        , ( show (nth1HList 2 mylist :: Maybe Bool)   -- shows Just True
        , ( show (nth1HList 3 mylist :: Maybe String) -- shows Just "42"
        )))) @=? output

output = ("Just 1",("Nothing",("Just True","Just \"42\"")))
