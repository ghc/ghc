{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Int
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The sized integral datatypes, 'Int8', 'Int16', 'Int32', and 'Int64'.
--

module GHC.Int
    (Int(..),
     Int8(..),
     Int16(..),
     Int32(..),
     Int64(..),
     uncheckedIShiftL64#,
     uncheckedIShiftRA64#,
     shiftRLInt8#,
     shiftRLInt16#,
     shiftRLInt32#,
     -- *  Equality operators
     -- |  See GHC.Classes#matching_overloaded_methods_in_rules
     eqInt,
     neInt,
     gtInt,
     geInt,
     ltInt,
     leInt,
     eqInt8,
     neInt8,
     gtInt8,
     geInt8,
     ltInt8,
     leInt8,
     eqInt16,
     neInt16,
     gtInt16,
     geInt16,
     ltInt16,
     leInt16,
     eqInt32,
     neInt32,
     gtInt32,
     geInt32,
     ltInt32,
     leInt32,
     eqInt64,
     neInt64,
     gtInt64,
     geInt64,
     ltInt64,
     leInt64
     ) where

import GHC.Internal.Int
