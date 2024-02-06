{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Handle
-- Copyright   :  (c) The University of Glasgow, 1994-2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable
--
-- External API for GHC's Handle implementation
--

module GHC.IO.Handle
    (Handle,
     BufferMode(..),
     mkFileHandle,
     mkDuplexHandle,
     hFileSize,
     hSetFileSize,
     hIsEOF,
     isEOF,
     hLookAhead,
     hSetBuffering,
     hSetBinaryMode,
     hSetEncoding,
     hGetEncoding,
     hFlush,
     hFlushAll,
     hDuplicate,
     hDuplicateTo,
     hClose,
     hClose_help,
     LockMode(..),
     hLock,
     hTryLock,
     HandlePosition,
     HandlePosn(..),
     hGetPosn,
     hSetPosn,
     SeekMode(..),
     hSeek,
     hTell,
     hIsOpen,
     hIsClosed,
     hIsReadable,
     hIsWritable,
     hGetBuffering,
     hIsSeekable,
     hSetEcho,
     hGetEcho,
     hIsTerminalDevice,
     hSetNewlineMode,
     Newline(..),
     NewlineMode(..),
     nativeNewline,
     noNewlineTranslation,
     universalNewlineMode,
     nativeNewlineMode,
     hShow,
     hWaitForInput,
     hGetChar,
     hGetLine,
     hGetContents,
     hGetContents',
     hPutChar,
     hPutStr,
     hGetBuf,
     hGetBufNonBlocking,
     hPutBuf,
     hPutBufNonBlocking
     ) where

import GHC.Internal.IO.Handle
