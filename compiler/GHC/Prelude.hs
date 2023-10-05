{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -O2 #-} -- See Note [-O2 Prelude]

-- | Custom GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.

-- Every module in GHC
--   * Is compiled with -XNoImplicitPrelude
--   * Explicitly imports GHC.Prelude

module GHC.Prelude
  (module GHC.Prelude
  ,module GHC.Utils.Trace
  ) where


{- Note [-O2 Prelude]
~~~~~~~~~~~~~~~~~~~~~
There is some code in GHC that is *always* compiled with -O[2] because
of it's impact on compile time performance. Some of this code might depend
on the definitions like shiftL being defined here being performant.

So we always compile this module with -O2. It's (currently) tiny so I
have little reason to suspect this impacts overall GHC compile times
negatively.

-}
-- We export the 'Semigroup' class but w/o the (<>) operator to avoid
-- clashing with the (Outputable.<>) operator which is heavily used
-- through GHC's code-base.

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

import GHC.Prelude.Basic as GHC.Prelude

-- import {-# SOURCE #-} GHC.Utils.Trace
import GHC.Utils.Trace hiding ( trace )
