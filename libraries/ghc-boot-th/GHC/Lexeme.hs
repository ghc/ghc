{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Lexeme
-- Copyright   :  (c) The GHC Team
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- Functions to evaluate whether or not a string is a valid identifier.
--
module GHC.Lexeme (
          -- * Lexical characteristics of Haskell names
        startsVarSym, startsVarId, startsConSym, startsConId,
        startsVarSymASCII, isVarSymChar, okSymChar
  ) where

import GHC.Internal.Lexeme
