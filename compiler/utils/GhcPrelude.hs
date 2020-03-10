{-# LANGUAGE CPP #-}

-- | Custom GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.

-- Every module in GHC
--   * Is compiled with -XNoImplicitPrelude
--   * Explicitly imports GhcPrelude

module GhcPrelude (module X) where

-- We export the 'Semigroup' class but w/o the (<>) operator to avoid
-- clashing with the (Outputable.<>) operator which is heavily used
-- through GHC's code-base.

import Prelude as X hiding ((<>))
import Data.Foldable as X (foldl')

{-
Note [Why do we import Prelude here?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The files ghc-boot-th.cabal, ghc-boot.cabal, ghci.cabal and
ghc-heap.cabal contain the directive default-extensions:
NoImplicitPrelude. There are two motivations for this:
  - Consistency with the compiler directory, which enables
    NoImplicitPrelude;
  - Allows loading the above dependent packages with ghc-in-ghci,
    giving a smoother development experience when adding new
    extensions.
-}
{-
Note [Depend on GhcPrelude]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
As detailed in Note [Depend on GHC.Integer], Note [Depend on GHC.Tuple], and
Note [Depend on GHC.Natural] (all in GHC.Base), we must be careful when
compiling any module before GHC.Base. We thus require every module not the
GHC.Base does not depend on to depend on GHC.Base. Normally, this dependency
happens naturally, but very small files might not otherwise depend on
GHC.Base. In particular, hs-boot files within GHC proper might have no
dependencies.

However, if we depend on GHC.Base directly, then we might try to build
even before the `base` package has been installed. Easiest is just to
depend here on GhcPrelude.
-}
