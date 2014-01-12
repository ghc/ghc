{-# OPTIONS -XRecursiveDo #-}

-- test of user defined instance of MonadFix

module Main (main) where

import Control.Monad.Fix

data X a = X a deriving Show

instance Monad X where
  return      = X
  (X a) >>= f = f a

instance MonadFix X where
  mfix f = fix (f . unX) 
         where unX ~(X x) = x

z :: X [Int]
z = mdo x <- return (1:x)
	return (take 4 x)

main = print z
