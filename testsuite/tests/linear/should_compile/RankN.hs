{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module RankN where
{-
inplace/bin/ghc-stage0 -O2 -dcore-lint
-}

import GHC.Base
{-
class Data a where
  gunfold :: (forall b r.  c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> c a

foo = 1
{-# NOINLINE foo #-}

instance Data [a] where
  gunfold k z = k (k (z (:)))


--qux :: Identity (Int ⊸ Int)
qux = Identity Just

app :: Identity (a -> b) -> Identity a -> Identity b
app (Identity f) (Identity a) = Identity (f a)

example = app qux (Identity 5)

--unqux :: Int ⊸ Int
unqux = Just

unapp :: (a -> b) -> a -> b
unapp f a = f a

example1 = unapp unqux 5

foo :: Identity (a -> b) -> a -> b
foo = runIdentity

fooTest = foo (Identity Just)

foo2 :: (a -> b) -> a -> b
foo2 = ($)

fooTest2 = let f = Just in foo2 f
-}

data Identity a = Identity { runIdentity :: a }

extraTest = (id Identity) :: a -> Identity a
