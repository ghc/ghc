{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}
module Dollar where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint

$ is not given a linear polymorphic type and so core lint complains
-}

import GHC.Base

data X = X

foo :: (X ⊸ X)
foo x = x

data Q a = Q a

data QU = QU ()

test = QU $ ()

qux = Q $ ()

