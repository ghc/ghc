{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

#if ! defined(mingw32_HOST_OS)

module GHC.IO.Encoding.CodePage ( ) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import Prelude () -- for build ordering

#else

module GHC.IO.Encoding.CodePage
  ( codePageEncoding, mkCodePageEncoding,
    localeEncoding, mkLocaleEncoding, CodePage,
    getCurrentCodePage
  ) where

import GHC.Internal.IO.Encoding.CodePage

#endif
