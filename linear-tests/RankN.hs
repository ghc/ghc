{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module RankN where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint
-}

import GHC.Base

class Data a where
  gunfold :: (forall b r.  c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> c a

foo = 1
{-# NOINLINE foo #-}

instance Data [a] where
  gunfold k z = k (k (z (:)))
