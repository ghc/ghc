{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.GHCi.Helpers
-- Copyright   :  (c) The GHC Developers
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable (GHC Extensions)
--
-- Various helpers used by the GHCi shell.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.GHCi.Helpers should be removed in GHC 10.02."
#endif

module GHC.GHCi.Helpers
  {-# DEPRECATED ["GHC.GHCi.Helpers is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context."] #-}
    (disableBuffering,
     flushAll,
     evalWrapper
     ) where

import GHC.Internal.GHCi.Helpers