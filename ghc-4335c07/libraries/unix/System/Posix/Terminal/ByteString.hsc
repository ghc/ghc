{-# LANGUAGE CApiFFI #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Terminal.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Terminal support
--
-----------------------------------------------------------------------------

module System.Posix.Terminal.ByteString (
  -- * Terminal support

  -- ** Terminal attributes
  TerminalAttributes,
  getTerminalAttributes,
  TerminalState(..),
  setTerminalAttributes,

  TerminalMode(..),
  withoutMode,
  withMode,
  terminalMode,
  bitsPerByte,
  withBits,

  ControlCharacter(..),
  controlChar,
  withCC,
  withoutCC,

  inputTime,
  withTime,
  minInput,
  withMinInput,

  BaudRate(..),
  inputSpeed,
  withInputSpeed,
  outputSpeed,
  withOutputSpeed,

  -- ** Terminal operations
  sendBreak,
  drainOutput,
  QueueSelector(..),
  discardData,
  FlowAction(..),
  controlFlow,

  -- ** Process groups
  getTerminalProcessGroupID,
  setTerminalProcessGroupID,

  -- ** Testing a file descriptor
  queryTerminal,
  getTerminalName,
  getControllingTerminalName,

  -- ** Pseudoterminal operations
  openPseudoTerminal,
  getSlaveTerminalName
  ) where

#include "HsUnix.h"

import Foreign
import System.Posix.Types
import System.Posix.Terminal.Common
#ifndef HAVE_OPENPTY
import System.Posix.IO.ByteString (defaultFileFlags, openFd, noctty, OpenMode(ReadWrite))
import Data.ByteString.Char8 as B ( pack, )
#endif

import Foreign.C hiding (
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_ )

import System.Posix.ByteString.FilePath

#if !HAVE_CTERMID
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

-- | @getTerminalName fd@ calls @ttyname@ to obtain a name associated
--   with the terminal for @Fd@ @fd@. If @fd@ is associated
--   with a terminal, @getTerminalName@ returns the name of the
--   terminal.
getTerminalName :: Fd -> IO RawFilePath
getTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getTerminalName" (c_ttyname fd)
  peekFilePath s

foreign import ccall unsafe "ttyname"
  c_ttyname :: CInt -> IO CString

-- | @getControllingTerminalName@ calls @ctermid@ to obtain
--   a name associated with the controlling terminal for the process.  If a
--   controlling terminal exists,
--   @getControllingTerminalName@ returns the name of the
--   controlling terminal.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @ctermid(3)@ (use @#if HAVE_CTERMID@ CPP guard to
-- detect availability).
getControllingTerminalName :: IO RawFilePath
#if HAVE_CTERMID
getControllingTerminalName = do
  s <- throwErrnoIfNull "getControllingTerminalName" (c_ctermid nullPtr)
  peekFilePath s

foreign import capi unsafe "termios.h ctermid"
  c_ctermid :: CString -> IO CString
#else
{-# WARNING getControllingTerminalName
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_CTERMID@)" #-}
getControllingTerminalName = ioError (ioeSetLocation unsupportedOperation "getControllingTerminalName")
#endif

-- | @getSlaveTerminalName@ calls @ptsname@ to obtain the name of the
-- slave terminal associated with a pseudoterminal pair.  The file
-- descriptor to pass in must be that of the master.
getSlaveTerminalName :: Fd -> IO RawFilePath

#ifdef HAVE_PTSNAME
getSlaveTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getSlaveTerminalName" (c_ptsname fd)
  peekFilePath s

# if __GLASGOW_HASKELL__ < 800
-- see comment in cbits/HsUnix.c
foreign import ccall unsafe "__hsunix_ptsname"
  c_ptsname :: CInt -> IO CString
# else
foreign import capi unsafe "HsUnix.h ptsname"
  c_ptsname :: CInt -> IO CString
# endif
#else
getSlaveTerminalName _ =
    ioError (errnoToIOError "getSlaveTerminalName" eNOSYS Nothing Nothing)
#endif

-- -----------------------------------------------------------------------------
-- openPseudoTerminal needs to be here because it depends on
-- getSlaveTerminalName.

-- | @openPseudoTerminal@ creates a pseudoterminal (pty) pair, and
-- returns the newly created pair as a (@master@, @slave@) tuple.
openPseudoTerminal :: IO (Fd, Fd)

#ifdef HAVE_OPENPTY
openPseudoTerminal =
  alloca $ \p_master ->
    alloca $ \p_slave -> do
      throwErrnoIfMinus1_ "openPty"
          (c_openpty p_master p_slave nullPtr nullPtr nullPtr)
      master <- peek p_master
      slave <- peek p_slave
      return (Fd master, Fd slave)

foreign import ccall unsafe "openpty"
  c_openpty :: Ptr CInt -> Ptr CInt -> CString -> Ptr CTermios -> Ptr a
            -> IO CInt
#else
openPseudoTerminal = do
  (Fd master) <- openFd (B.pack "/dev/ptmx") ReadWrite Nothing
                        defaultFileFlags{noctty=True}
  throwErrnoIfMinus1_ "openPseudoTerminal" (c_grantpt master)
  throwErrnoIfMinus1_ "openPseudoTerminal" (c_unlockpt master)
  slaveName <- getSlaveTerminalName (Fd master)
  slave <- openFd slaveName ReadWrite Nothing defaultFileFlags{noctty=True}
  pushModule slave "ptem"
  pushModule slave "ldterm"
# ifndef __hpux
  pushModule slave "ttcompat"
# endif /* __hpux */
  return (Fd master, slave)

-- Push a STREAMS module, for System V systems.
pushModule :: Fd -> String -> IO ()
pushModule (Fd fd) name =
  withCString name $ \p_name ->
    throwErrnoIfMinus1_ "openPseudoTerminal"
                        (c_push_module fd p_name)

foreign import ccall unsafe "__hsunix_push_module"
  c_push_module :: CInt -> CString -> IO CInt

#if HAVE_PTSNAME
# if __GLASGOW_HASKELL__ < 800
-- see comment in cbits/HsUnix.c
foreign import ccall unsafe "__hsunix_grantpt"
  c_grantpt :: CInt -> IO CInt

foreign import ccall unsafe "__hsunix_unlockpt"
  c_unlockpt :: CInt -> IO CInt
# else
foreign import capi unsafe "HsUnix.h grantpt"
  c_grantpt :: CInt -> IO CInt

foreign import capi unsafe "HsUnix.h unlockpt"
  c_unlockpt :: CInt -> IO CInt
# endif
#else
c_grantpt :: CInt -> IO CInt
c_grantpt _ = return (fromIntegral (0::Int))

c_unlockpt :: CInt -> IO CInt
c_unlockpt _ = return (fromIntegral (0::Int))
#endif /* HAVE_PTSNAME */
#endif /* !HAVE_OPENPTY */
