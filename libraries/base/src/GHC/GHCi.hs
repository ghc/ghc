{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.GHCi
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable (GHC Extensions)
--
-- The GHCi Monad lifting interface.
--
-- EXPERIMENTAL! DON'T USE.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.GHCi should be removed in GHC 10.02."
#endif

module GHC.GHCi
  {-# DEPRECATED ["GHC.GHCi is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context."] #-}
    {-# WARNING "This is an unstable interface." #-}
    (GHCiSandboxIO(..),
     NoIO()
     ) where

import GHC.Internal.GHCi
