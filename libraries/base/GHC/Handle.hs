{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Handle
-- Copyright   :  (c) The University of Glasgow, 1994-2001
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Backwards-compatibility interface
--
-----------------------------------------------------------------------------

-- #hide
module GHC.Handle {-# DEPRECATED "use GHC.IO.Handle instead" #-} (
  withHandle, withHandle', withHandle_,
  wantWritableHandle, wantReadableHandle, wantSeekableHandle,

--  newEmptyBuffer, allocateBuffer, readCharFromBuffer, writeCharIntoBuffer,
--  flushWriteBufferOnly, flushWriteBuffer,
--  flushReadBuffer,
--  fillReadBuffer, fillReadBufferWithoutBlocking,
--  readRawBuffer, readRawBufferPtr,
--  readRawBufferNoBlock, readRawBufferPtrNoBlock,
--  writeRawBuffer, writeRawBufferPtr,

  ioe_closedHandle, ioe_EOF, ioe_notReadable, ioe_notWritable,

  stdin, stdout, stderr,
  IOMode(..), openFile, openBinaryFile, 
--  fdToHandle_stat,
  fdToHandle, fdToHandle',
  hFileSize, hSetFileSize, hIsEOF, isEOF, hLookAhead, hLookAhead_, 
  hSetBuffering, hSetBinaryMode,
  hFlush, hDuplicate, hDuplicateTo,

  hClose, hClose_help,

  HandlePosition, HandlePosn(..), hGetPosn, hSetPosn,
  SeekMode(..), hSeek, hTell,

  hIsOpen, hIsClosed, hIsReadable, hIsWritable, hGetBuffering, hIsSeekable,
  hSetEcho, hGetEcho, hIsTerminalDevice,

  hShow,

 ) where

import GHC.IO.IOMode
import GHC.IO.Handle
import GHC.IO.Handle.Internals
import GHC.IO.Handle.FD

