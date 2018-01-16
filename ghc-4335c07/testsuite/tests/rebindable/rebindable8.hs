{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses #-}

-- Trac #1537

module Foo where
import Prelude hiding (Monad(..))

class Bind m1 m2 m3 where 
  (>>=) :: m1 a -> (a -> m2 b) -> m3 b

class Return m where
  return :: a -> m a
  fail   :: String -> m a 

instance Bind Maybe [] [] where
  Just x  >>= f = f x
  Nothing >>= f = []

instance Return [] where 
  return x = [x]
  fail _   = [] 

should_compile :: [Int]
should_compile = do 
  a <- Just 1
  [a] 
