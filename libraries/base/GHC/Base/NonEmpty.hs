{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base.NonEmpty
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The 'NonEmpty' type.
--
-----------------------------------------------------------------------------

module GHC.Base.NonEmpty
    ( NonEmpty(..)
    ) where

-- ghc-prim
import GHC.Classes

infixr 5 :|

-- | Non-empty (and non-strict) list type.
--
-- @since 4.9.0.0
data NonEmpty a = a :| [a]
  deriving ( Eq  -- ^ @since 4.9.0.0
           , Ord -- ^ @since 4.9.0.0
           )
