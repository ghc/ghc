-- |
--
-- Module      :  Foreign.C.Types
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Mapping of C types to corresponding Haskell types.
--

module Foreign.C.Types
    (-- *  Representations of C types
     -- $ctypes
     -- **  #platform# Platform differences
     -- |  This module contains platform specific information about types.
     -- __/As such, the types presented on this page reflect the/__
     -- __/platform on which the documentation was generated and may/__
     -- __/not coincide with the types on your platform./__
     -- **  Integral types
     -- |  These types are represented as @newtype@s of
     -- types in "Data.Int" and "Data.Word", and are instances of
     -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
     -- 'Prelude.Show', 'Prelude.Enum', 'Data.Typeable.Typeable',
     -- 'Storable', 'Prelude.Bounded', 'Prelude.Real', 'Prelude.Integral'
     -- and 'Bits'.
     CChar(..),
     CSChar(..),
     CUChar(..),
     CShort(..),
     CUShort(..),
     CInt(..),
     CUInt(..),
     CLong(..),
     CULong(..),
     CPtrdiff(..),
     CSize(..),
     CWchar(..),
     CSigAtomic(..),
     CLLong(..),
     CULLong(..),
     CBool(..),
     CIntPtr(..),
     CUIntPtr(..),
     CIntMax(..),
     CUIntMax(..),
     -- **  Numeric types
     -- |  These types are represented as @newtype@s of basic
     -- foreign types, and are instances of
     -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
     -- 'Prelude.Show', 'Prelude.Enum', 'Data.Typeable.Typeable' and
     -- 'Storable'.
     CClock(..),
     CTime(..),
     CUSeconds(..),
     CSUSeconds(..),
     -- |  To convert 'CTime' to 'Data.Time.UTCTime', use the following:
     --
     -- > \t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)

     -- **  Floating types
     -- |  These types are represented as @newtype@s of
     -- 'Prelude.Float' and 'Prelude.Double', and are instances of
     -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
     -- 'Prelude.Show', 'Prelude.Enum', 'Data.Typeable.Typeable', 'Storable',
     -- 'Prelude.Real', 'Prelude.Fractional', 'Prelude.Floating',
     -- 'Prelude.RealFrac' and 'Prelude.RealFloat'. That does mean
     -- that `CFloat`'s (respectively `CDouble`'s) instances of
     -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num' and
     -- 'Prelude.Fractional' are as badly behaved as `Prelude.Float`'s
     -- (respectively `Prelude.Double`'s).
     CFloat(..),
     CDouble(..),
     -- XXX GHC doesn't support CLDouble yet
     -- , CLDouble(..)
     -- **  Other types
     CFile,
     CFpos,
     CJmpBuf
     ) where

import GHC.Internal.Foreign.C.Types
