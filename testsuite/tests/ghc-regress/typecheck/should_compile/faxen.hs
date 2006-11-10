{-# OPTIONS -fglasgow-exts #-}

-- A classic test for type inference
-- Taken from "Haskell and principal types", Section 3
-- by Faxen, in the Haskell Workshop 2003, pp88-97

module ShouldCompile where

class HasEmpty a where
  isEmpty :: a -> Bool

instance HasEmpty [a] where
  isEmpty x = null x

instance HasEmpty (Maybe a) where
  isEmpty Nothing  = True
  isEmpty (Just x) = False

test1 y 
  =  (null y)
  || (let f :: forall d. d -> Bool
	  f x = isEmpty (y >> return x)
      in f y)

test2 y 
  =  (let f :: forall d. d -> Bool
	  f x = isEmpty (y >> return x)
      in f y)
  || (null y)

