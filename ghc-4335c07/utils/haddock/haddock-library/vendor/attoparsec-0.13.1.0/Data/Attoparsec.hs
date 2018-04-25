-- |
-- Module      :  Data.Attoparsec
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for
-- 'Data.ByteString.ByteString' strings, loosely based on the Parsec
-- library.
--
-- This module is deprecated. Use "Data.Attoparsec.ByteString"
-- instead.

module Data.Attoparsec
    {-# DEPRECATED "This module will be removed in the next major release." #-}
    (
      module Data.Attoparsec.ByteString
    ) where

import Data.Attoparsec.ByteString
