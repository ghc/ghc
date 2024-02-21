{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Desugar
-- Copyright   :  (c) The University of Glasgow, 2007
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Support code for desugaring in GHC
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.Desugar
    ((>>>),
     AnnotationWrapper(..),
     toAnnotationWrapper
     ) where

import GHC.Internal.Desugar
