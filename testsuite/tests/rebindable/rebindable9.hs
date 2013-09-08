{-# LANGUAGE RebindableSyntax, FlexibleInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- Trac #1537

module Foo where
import qualified Prelude
import Prelude hiding (Monad(..))

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

newtype Identity a = Identity { runIdentity :: a }

instance Prelude.Functor Identity where
    fmap = liftM

instance Applicative Identity where
    pure = Prelude.return
    (<*>) = ap

instance Prelude.Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

class Bind m1 m2 m3 | m1 m2 -> m3 where 
  (>>=) :: m1 a -> (a -> m2 b) -> m3 b

class Return m where
  returnM :: a -> m a
  fail   :: String -> m a 

instance Bind Maybe [] [] where
  Just x  >>= f = f x
  Nothing >>= f = []

instance Functor a => Bind Identity a a 	where m >>= f = f (runIdentity m)
instance Functor a => Bind a Identity a 	where m >>= f = fmap (runIdentity . f) m

instance Prelude.Monad m => Bind m m m where (>>=) = (Prelude.>>=)

instance Return [] where 
  returnM x = [x]
  fail _   = [] 

return :: a -> Identity a
return = Prelude.return

should_compile :: [Int]
should_compile = do 
  a <- Just 1
  b <- [a*1,a*2]
  return (b+1)