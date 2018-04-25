{-# OPTIONS -XRecursiveDo #-}

-- OLD: mdo requires MonadFix instance, even
--      if no recursion is present

-- Dec 2010: Small change of behaviour
--     MonadFix is only required if recursion is present

module Main (main) where

import Control.Monad.Fix

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data X a = X a deriving Show

instance Functor X where
    fmap = liftM

instance Applicative X where
    pure = return
    (<*>) = ap

instance Monad X where
  return      = X
  (X a) >>= f = f a

z :: X [Int]
z = mdo { a <- return 1; return [a] }

main = print z
