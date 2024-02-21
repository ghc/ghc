{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Show
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Show' class, and related operations.
--

module GHC.Show
        (
        Show(..), ShowS,

        -- * Show support code
        shows, showChar, showString, showMultiLineString,
        showParen, showList__, showCommaSpace, showSpace,
        showLitChar, showLitString, protectEsc,
        intToDigit, showSignedInt,
        appPrec, appPrec1,

        -- * Character operations
        asciiTab,
  ) where

import GHC.Internal.Show
