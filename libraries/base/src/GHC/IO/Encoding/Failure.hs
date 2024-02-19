{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}

-- |
--
-- Module      :  GHC.IO.Encoding.Failure
-- Copyright   :  (c) The University of Glasgow, 2008-2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Types for specifying how text encoding/decoding fails
--

module GHC.IO.Encoding.Failure
    (CodingFailureMode(..),
     codingFailureModeSuffix,
     isSurrogate,
     recoverDecode,
     recoverEncode,
     recoverDecode#,
     recoverEncode#
     ) where

import GHC.Internal.IO.Encoding.Failure
