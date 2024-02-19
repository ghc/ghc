{-# LANGUAGE Safe #-}

-- |
-- Module      :  Foreign.C.String
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for primitive marshalling of C strings.
--
-- The marshalling converts each Haskell character, representing a Unicode
-- code point, to one or more bytes in a manner that, by default, is
-- determined by the current locale.  As a consequence, no guarantees
-- can be made about the relative length of a Haskell string and its
-- corresponding C string, and therefore all the marshalling routines
-- include memory allocation.  The translation between Unicode and the
-- encoding of the current locale may be lossy.
--

module Foreign.C.String (
  -- * C strings

  CString,
  CStringLen,

  -- ** Using a locale-dependent encoding

  -- | These functions are different from their @CAString@ counterparts
  -- in that they will use an encoding determined by the current locale,
  -- rather than always assuming ASCII.

  -- conversion of C strings into Haskell strings
  --
  peekCString,
  peekCStringLen,

  -- conversion of Haskell strings into C strings
  --
  newCString,
  newCStringLen,

  -- conversion of Haskell strings into C strings using temporary storage
  --
  withCString,
  withCStringLen,

  charIsRepresentable,

  -- ** Using 8-bit characters

  -- | These variants of the above functions are for use with C libraries
  -- that are ignorant of Unicode.  These functions should be used with
  -- care, as a loss of information can occur.

  castCharToCChar,
  castCCharToChar,

  castCharToCUChar,
  castCUCharToChar,
  castCharToCSChar,
  castCSCharToChar,

  peekCAString,
  peekCAStringLen,
  newCAString,
  newCAStringLen,
  withCAString,
  withCAStringLen,

  -- * C wide strings

  -- | These variants of the above functions are for use with C libraries
  -- that encode Unicode using the C @wchar_t@ type in a system-dependent
  -- way.  The only encodings supported are
  --
  -- * UTF-32 (the C compiler defines @__STDC_ISO_10646__@), or
  --
  -- * UTF-16 (as used on Windows systems).

  CWString,
  CWStringLen,

  peekCWString,
  peekCWStringLen,
  newCWString,
  newCWStringLen,
  withCWString,
  withCWStringLen,

  ) where

import GHC.Internal.Foreign.C.String
