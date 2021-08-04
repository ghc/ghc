-- |
-- Module      :  Data.Attoparsec.Char8
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, character-oriented combinator parsing for
-- 'Data.ByteString.ByteString' strings, loosely based on the Parsec
-- library.
--
-- This module is deprecated. Use "Data.Attoparsec.ByteString.Char8"
-- instead.

module Data.Attoparsec.Char8
    {-# DEPRECATED "This module will be removed in the next major release." #-}
    (
      module Data.Attoparsec.ByteString.Char8
    ) where

import Data.Attoparsec.ByteString.Char8
