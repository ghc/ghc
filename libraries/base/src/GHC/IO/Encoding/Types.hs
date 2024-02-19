{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
--
-- Module      :  GHC.IO.Encoding.Types
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Types for text encoding/decoding
--

module GHC.IO.Encoding.Types
    (BufferCodec(..),
     TextEncoding(..),
     TextEncoder,
     TextDecoder,
     CodeBuffer,
     EncodeBuffer,
     DecodeBuffer,
     CodingProgress(..),
     DecodeBuffer#,
     EncodeBuffer#,
     DecodingBuffer#,
     EncodingBuffer#
     ) where

import GHC.Internal.IO.Encoding.Types
