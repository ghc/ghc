{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Read
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Read' class and instances for basic data types.
--

module GHC.Read
  ( -- * Class
    Read(..)

  -- * ReadS type
  , ReadS

  -- * Haskell 2010 compatibility
  , lex
  , lexLitChar
  , readLitChar
  , lexDigits

  -- * Defining readers
  , lexP, expectP
  , paren
  , parens
  , list
  , choose
  , readListDefault, readListPrecDefault
  , readNumber
  , readField
  , readFieldHash
  , readSymField

  , readParen
  )
 where

import GHC.Internal.Read
