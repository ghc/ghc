{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
--
-- Module      :  GHC.Desugar
-- Copyright   :  (c) The University of Glasgow, 2007
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/216>)
-- Portability :  non-portable (GHC extensions)
--
-- Support code for desugaring in GHC
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

#if __GLASGOW_HASKELL >= 914
#error "GHC.Desugar should be removed in GHC 9.14"
#endif

module GHC.Desugar
  {-# DEPRECATED ["GHC.Desugar is deprecated and will be removed in GHC 9.14.", "(>>>) should be imported from Control.Arrow.", "AnnotationWrapper is internal to GHC and should not be used externally."] #-}
  ((>>>), AnnotationWrapper(..), toAnnotationWrapper) where

import GHC.Internal.Desugar
