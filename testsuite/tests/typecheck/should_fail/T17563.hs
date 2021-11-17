{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE QuantifiedConstraints #-}

module T17563 where

data T a b

blah :: (forall x. Num (T a x)) => T a b
blah = 0
