module GHC.Types.Var where

import GHC.Prelude ()
  -- We compile this module with -XNoImplicitPrelude (for some
  -- reason), so if there are no imports it does not seem to
  -- depend on anything.  But it does! We must, for example,
  -- compile GHC.Types in the ghc-prim library first.
  -- So this otherwise-unnecessary import tells the build system
  -- that this module depends on GHC.Prelude, which ensures
  -- that GHC.Type is built first.

data ArgFlag
data AnonArgFlag
data Var
