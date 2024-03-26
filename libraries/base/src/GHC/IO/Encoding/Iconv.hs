{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.IO.Encoding.Iconv
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This module provides text encoding/decoding using iconv
--

module GHC.IO.Encoding.Iconv
#if !defined(mingw32_HOST_OS)
    (iconvEncoding,
     mkIconvEncoding,
     localeEncodingName
     ) where

import GHC.Internal.IO.Encoding.Iconv

#else
    ( ) where

import Prelude () -- for build ordering (#23942)
#endif
