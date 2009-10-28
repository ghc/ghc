{-# OPTIONS -XRecursiveDo #-}

-- mdo requires MonadFix instance, even
-- if no recursion is present

module Main (main) where

import Control.Monad.Fix

data X a = X a deriving Show

instance Monad X where
  return      = X
  (X a) >>= f = f a

z :: X [Int]
z = mdo return [1,2,3,4]

main = print z
