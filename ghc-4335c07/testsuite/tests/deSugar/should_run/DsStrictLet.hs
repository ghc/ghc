{-# LANGUAGE Strict #-}
module Main where

import Debug.Trace

main = let False = trace "no binders" False -- evaluated

           a :: a -> a
           a = trace "polymorphic" id -- evaluated

           f :: Eq a => a -> a -> Bool
           f = trace "overloaded" (==) -- not evaluated

           xs :: [Int]
           xs = (trace "recursive" (:) 1 xs) -- evaluated
       in return ()
