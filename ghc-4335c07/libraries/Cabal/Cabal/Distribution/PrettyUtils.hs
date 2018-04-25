-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PrettyUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for pretty printing.
{-# OPTIONS_HADDOCK hide #-}
module Distribution.PrettyUtils {-# DEPRECATED "Use Distribution.Pretty" #-} (
    Separator,
    -- * Internal
    showFilePath,
    showToken,
    showTestedWith,
    showFreeText,
    indentWith,
    ) where

import Distribution.Pretty
import Distribution.ParseUtils
