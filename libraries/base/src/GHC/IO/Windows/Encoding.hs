-- |
-- Module      :  System.Win32.Encoding
-- Copyright   :  2012 shelarcy
-- License     :  BSD-style
--
-- Maintainer  :  shelarcy@gmail.com
-- Stability   :  Provisional
-- Portability :  Non-portable (Win32 API)
--
-- Encode/Decode multibyte character using Win32 API.
--

module GHC.IO.Windows.Encoding
  ( encodeMultiByte
  , encodeMultiByteIO
  , encodeMultiByteRawIO
  , decodeMultiByte
  , decodeMultiByteIO
  , wideCharToMultiByte
  , multiByteToWideChar
  , withGhcInternalToUTF16
  , withUTF16ToGhcInternal
  ) where

import GHC.Internal.IO.Windows.Encoding
