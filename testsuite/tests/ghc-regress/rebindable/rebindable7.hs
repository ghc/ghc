{-# OPTIONS -fno-implicit-prelude #-}

-- This one tests rebindable syntax for do-notation

module Main where

import qualified Prelude
import GHC.Num
import GHC.Base hiding( Monad(..) )

class Foo a where
  op :: a -> a

data T a = MkT a 

instance Foo Int where
  op x = x+1

(>>=) :: Foo a => T a -> (a -> T b) -> T b
(>>=) (MkT x) f = f (op x)

(>>) :: Foo a => T a -> T b -> T b
(>>) x y = x >>= (\_ -> y)

return :: Num a => a -> T a
return x = MkT (x+1)

fail :: String -> T a
fail s = error "urk"

t1 :: T Int
t1 = MkT 4

myt = do { x <- t1
	 ; return x }

main = case myt of
	 MkT i -> Prelude.print i
