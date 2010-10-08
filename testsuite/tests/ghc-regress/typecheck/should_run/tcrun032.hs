
{-# LANGUAGE UndecidableInstances #-}

-- This tests the recursive-dictionary stuff.

module Main where

data Fix f = In (f (Fix f)) 

instance Show (f (Fix f)) => Show (Fix f) where
  show (In x) = "In " ++ show x	-- No parens, but never mind

instance Eq (f (Fix f)) => Eq (Fix f) where
  (In x) == (In y) = x==y

data L x = Nil | Cons Int x  deriving( Show, Eq )

main = do { print (In Nil); 
	    print (In Nil == In Nil) }

