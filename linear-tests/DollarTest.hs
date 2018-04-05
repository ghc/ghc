{-# LANGUAGE NoImplicitPrelude #-}
module Dollar where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint

$ is not given a linear polymorphic type and so core lint complains
-}

import GHC.Base

data X = X

foo :: (X ⊸ X)
foo x = x

qux = foo $ X
