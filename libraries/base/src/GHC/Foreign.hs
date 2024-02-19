{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.Foreign
-- Copyright   :  (c) The University of Glasgow, 2008-2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Foreign marshalling support for CStrings with configurable encodings
--

module GHC.Foreign
    (-- *  C strings with a configurable encoding
     CString,
     CStringLen,
     -- *  Conversion of C strings into Haskell strings
     peekCString,
     peekCStringLen,
     -- *  Conversion of Haskell strings into C strings
     newCString,
     newCStringLen,
     newCStringLen0,
     -- *  Conversion of Haskell strings into C strings using temporary storage
     withCString,
     withCStringLen,
     withCStringLen0,
     withCStringsLen,
     charIsRepresentable
     ) where

import GHC.Internal.Foreign.C.String.Encoding
