{-# OPTIONS -fglasgow-exts #-}

{-

This module illustrates heterogeneously typed lists.

-}

module Main where
import Data.Typeable


-- Heterogeneously typed lists
data HList = HNil
           | forall a. Typeable a => HCons a HList

-- The empty list
initHList :: HList
initHList = HNil

-- Add an entry
addHList :: Typeable a => a -> HList -> HList
addHList a l = HCons a l

-- Test for an empty list
nullHList :: HList -> Bool
nullHList HNil = True
nullHList (HCons _ _) = False

-- Retrieve head by type case
headHList :: Typeable a => HList -> Maybe a
headHList HNil = Nothing
headHList (HCons a _) = cast a

-- Retrieve head by type case
tailHList :: HList -> HList
tailHList HNil = error "tailHList"
tailHList (HCons _ l) = l

-- Access per index; starts at 1
nth1HList :: Typeable a => Int -> HList -> Maybe a
nth1HList i l | i < 1 || i == 0 && nullHList l = error "nth1HList"
nth1HList 1 l = headHList l
nth1HList i l = nth1HList (i-1) (tailHList l)

----------------------------------------------------------------------------

-- A demo list
mylist = addHList (1::Int)       $
         addHList (True::Bool)   $
         addHList ("42"::String) $
         initHList

-- Main function for testing
main = print   ( show (nth1HList 1 mylist :: Maybe Int)    -- shows Just 1
             , ( show (nth1HList 1 mylist :: Maybe Bool)   -- shows Nothing
             , ( show (nth1HList 2 mylist :: Maybe Bool)   -- shows Just True
             , ( show (nth1HList 3 mylist :: Maybe String) -- shows Just "42"
             ))))
