{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Encoding.Latin1
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Single-byte encodings that map directly to Unicode code points.
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--

module GHC.IO.Encoding.Latin1
    (latin1,
     mkLatin1,
     latin1_checked,
     mkLatin1_checked,
     ascii,
     mkAscii,
     latin1_decode,
     ascii_decode,
     latin1_encode,
     latin1_checked_encode,
     ascii_encode
     ) where

import GHC.Internal.IO.Encoding.Latin1