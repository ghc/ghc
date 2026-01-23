{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.IO.Handle.Types
-- Copyright   :  (c) The University of Glasgow, 1994-2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable
--
-- Basic types for the implementation of IO Handles.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.Handle.Types should be removed in GHC 10.02."
#endif

module GHC.IO.Handle.Types (
  {-# DEPRECATED ["GHC.IO.Handle.Types is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context."] #-}
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
