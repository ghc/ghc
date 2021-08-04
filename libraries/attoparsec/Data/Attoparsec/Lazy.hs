-- |
-- Module      :  Data.Attoparsec.Lazy
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for lazy 'ByteString'
-- strings, loosely based on the Parsec library.
--
-- This is essentially the same code as in the 'Data.Attoparsec'
-- module, only with a 'parse' function that can consume a lazy
-- 'ByteString' incrementally, and a 'Result' type that does not allow
-- more input to be fed in.  Think of this as suitable for use with a
-- lazily read file, e.g. via 'L.readFile' or 'L.hGetContents'.
--
-- Behind the scenes, strict 'B.ByteString' values are still used
-- internally to store parser input and manipulate it efficiently.
-- High-performance parsers such as 'string' still expect strict
-- 'B.ByteString' parameters.

module Data.Attoparsec.Lazy
    (
      module Data.Attoparsec.ByteString.Lazy
    ) where

import Data.Attoparsec.ByteString.Lazy
