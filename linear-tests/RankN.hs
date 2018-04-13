{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module RankN where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint

$ is not given a linear polymorphic type and so core lint complains
-}

import GHC.Base

class Data a where
  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> c a

foo = 1
{-# NOINLINE foo #-}

instance Data a => Data [a] where
  gunfold k z = case foo of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> errorWithoutStackTrace "Data.Data.gunfold(List)"
