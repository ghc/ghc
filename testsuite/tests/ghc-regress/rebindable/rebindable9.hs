{-# OPTIONS -fno-implicit-prelude -fglasgow-exts #-}

-- Trac #1537

module Foo where
import qualified Prelude
import Prelude hiding (Monad(..))
import Control.Monad.Identity (Identity(..))

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