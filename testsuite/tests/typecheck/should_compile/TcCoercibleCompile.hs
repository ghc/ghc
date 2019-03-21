{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -fwarn-unused-imports  #-}

import Data.Coerce
import Data.Proxy
import Data.Monoid (First(First)) -- check whether the implicit use of First is noted

-- see https://gitlab.haskell.org/ghc/ghc/wikis/design/new-coercible-solver/v2

foo1 :: f a -> f a
foo1 = coerce

newtype X = MkX (Int -> X)
foo2 :: X -> X
foo2 = coerce

newtype X2 a = MkX2 Char
type role X2 nominal

foo3 :: X2 Int -> X2 Bool
foo3 = coerce

newtype Age = MkAge Int
newtype Y a = MkY a
type role Y nominal

foo4 :: Y Age -> Y Int
foo4 = coerce

newtype Z a = MkZ ()
type role Z representational

foo5 :: Z Int -> Z Bool
foo5 = coerce

newtype App f a = MkApp (f a)

foo6 :: f a -> App f a
foo6 = coerce

foo7 :: Coercible a b => b -> a
foo7 = coerce

foo8 :: (Coercible a b, Coercible b c) => Proxy b -> a -> c
foo8 _ = coerce

main = print (coerce $ Just (1::Int)  :: First Int)



