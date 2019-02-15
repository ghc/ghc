-- Var.hs-boot is Imported (only) by TyCoRep.hs-boot
module Var where

import GhcPrelude ()
  -- We compile this module with -XNoImplicitPrelude (for some
  -- reason), so if there are no imports it does not seem to
  -- depend on anything.  But it does! We must, for example,
  -- compile GHC.Types in the ghc-prim library first.
  -- So this otherwise-unnecessary import tells the build system
  -- that this module depends on GhcPrelude, which ensures
  -- that GHC.Type is built first.

data ArgFlag
data AnonArgFlag
data Var
