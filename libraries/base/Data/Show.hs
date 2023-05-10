{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Show
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'Show' class.
--
-----------------------------------------------------------------------------

module Data.Show
    ( Show(..)
      -- * 'ShowS'
    , ShowS
    , shows
    , showChar, showString, showMultiLineString
    , showParen, showCommaSpace, showSpace
    , showLitChar, showLitString
    ) where

import GHC.Show

