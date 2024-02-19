{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

#if ! defined(mingw32_HOST_OS)

module GHC.IO.Encoding.CodePage ( ) where

import Prelude () -- for build ordering

#else

module GHC.IO.Encoding.CodePage
  ( codePageEncoding, mkCodePageEncoding,
    localeEncoding, mkLocaleEncoding, CodePage,
    getCurrentCodePage
  ) where

import GHC.Internal.IO.Encoding.CodePage

#endif
