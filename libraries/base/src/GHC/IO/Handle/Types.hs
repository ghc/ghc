{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.IO.Handle.Types
-- Copyright   :  (c) The University of Glasgow, 1994-2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Basic types for the implementation of IO Handles.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.IO.Handle.Types (
      Handle(..), Handle__(..), showHandle,
      checkHandleInvariants,
      BufferList(..),
      HandleType(..),
      isReadableHandleType, isWritableHandleType, isReadWriteHandleType,
      isAppendHandleType,
      BufferMode(..),
      BufferCodec(..),
      NewlineMode(..), Newline(..), nativeNewline,
      universalNewlineMode, noNewlineTranslation, nativeNewlineMode
  ) where

import GHC.Internal.IO.Handle.Types
