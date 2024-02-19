{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Encoding.UTF32
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- UTF-32 Codecs for the IO library
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--

module GHC.IO.Encoding.UTF32
    (utf32,
     mkUTF32,
     utf32_decode,
     utf32_encode,
     utf32be,
     mkUTF32be,
     utf32be_decode,
     utf32be_encode,
     utf32le,
     mkUTF32le,
     utf32le_decode,
     utf32le_encode
     ) where

import GHC.Internal.IO.Encoding.UTF32