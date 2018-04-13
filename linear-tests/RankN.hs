{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module RankN where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint
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
-}

data Identity a = Identity a

qux :: Identity (Int ⊸ Int)
qux = Identity (\x -> x)

app :: Identity (a -> b) -> Identity a -> Identity b
app (Identity f) (Identity a) = Identity (f a)

example = app qux (Identity 5)

unqux :: Int ⊸ Int
unqux = \x -> x

unapp :: (a -> b) -> a -> b
unapp f a = f a

example1 = unapp unqux 5
