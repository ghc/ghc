{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Encoding
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable
--
-- Text codecs for I/O
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.Encoding should be removed in GHC 10.02."
#endif

module GHC.IO.Encoding
  {-# DEPRECATED ["GHC.IO.Encoding is deprecated and will be removed in GHC 10.02. Please use the ghc-internal package."] #-}
    (BufferCodec(..),
     TextEncoding(..),
     TextEncoder,
     TextDecoder,
     CodingProgress(..),
     latin1,
     latin1_encode,
     latin1_decode,
     utf8,
     utf8_bom,
     utf16,
     utf16le,
     utf16be,
     utf32,
     utf32le,
     utf32be,
     initLocaleEncoding,
     getLocaleEncoding,
     getFileSystemEncoding,
     getForeignEncoding,
     setLocaleEncoding,
     setFileSystemEncoding,
     setForeignEncoding,
     char8,
     mkTextEncoding,
     argvEncoding
     ) where

import GHC.Internal.IO.Encoding