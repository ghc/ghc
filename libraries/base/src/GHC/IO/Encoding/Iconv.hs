{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
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

#if !defined(mingw32_HOST_OS)
module GHC.IO.Encoding.Iconv
    (iconvEncoding,
     mkIconvEncoding,
     localeEncodingName
     ) where

import GHC.Internal.IO.Encoding.Iconv

#else
module GHC.IO.Encoding.Iconv ( ) where

import GHC.Internal.Base () -- For build ordering

#endif
