{-# LANGUAGE MagicHash #-}

-- |
--
-- Module      :  GHC.Encoding.UTF8
-- Copyright   :  (c) The University of Glasgow, 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- Simple UTF-8 codecs supporting non-streaming encoding/decoding.
-- For encoding where codepoints may be broken across buffers,
-- see "GHC.IO.Encoding.UTF8".
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--

module GHC.Encoding.UTF8
    (-- *  Decoding single characters
     utf8DecodeCharAddr#,
     utf8DecodeCharPtr,
     utf8DecodeCharByteArray#,
     -- *  Decoding strings
     utf8DecodeByteArray#,
     utf8DecodeForeignPtr,
     -- *  Counting characters
     utf8CountCharsByteArray#,
     -- *  Comparison
     utf8CompareByteArray#,
     -- *  Encoding strings
     utf8EncodePtr,
     utf8EncodeByteArray#,
     utf8EncodedLength
     ) where

import GHC.Internal.Encoding.UTF8
