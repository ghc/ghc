{-# LANGUAGE CPP #-}

{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.IO.Handle.Internals
-- Copyright   :  (c) The University of Glasgow, 1994-2001
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable
--
-- This module defines the basic operations on I\/O \"handles\".  All
-- of the operations defined here are independent of the underlying
-- device.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.Handle.Internals should be removed in GHC 10.02."
#endif

module GHC.IO.Handle.Internals
  {-# DEPRECATED "GHC.IO.Handle.Internals is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context." #-}
    (withHandle,
     withHandle',
     withHandle_,
     withHandle__',
     withHandle_',
     withAllHandles__,
     wantWritableHandle,
     wantReadableHandle,
     wantReadableHandle_,
     wantSeekableHandle,
     mkHandle,
     mkFileHandle,
     mkFileHandleNoFinalizer,
     mkDuplexHandle,
     mkDuplexHandleNoFinalizer,
     addHandleFinalizer,
     openTextEncoding,
     closeTextCodecs,
     initBufferState,
     dEFAULT_CHAR_BUFFER_SIZE,
     flushBuffer,
     flushWriteBuffer,
     flushCharReadBuffer,
     flushCharBuffer,
     flushByteReadBuffer,
     flushByteWriteBuffer,
     readTextDevice,
     writeCharBuffer,
     readTextDeviceNonBlocking,
     decodeByteBuf,
     augmentIOError,
     ioe_closedHandle,
     ioe_semiclosedHandle,
     ioe_EOF,
     ioe_notReadable,
     ioe_notWritable,
     ioe_finalizedHandle,
     ioe_bufsiz,
     hClose_impl,
     hClose_help,
     hLookAhead_,
     HandleFinalizer,
     handleFinalizer,
     debugIO,
     traceIO
     ) where

import GHC.Internal.IO.Handle.Internals
