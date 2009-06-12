{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IOBase
-- Copyright   :  (c) The University of Glasgow 1994-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Backwards-compatibility interface
--
-----------------------------------------------------------------------------


module GHC.IOBase {-# DEPRECATED "use GHC.IO instead" #-} (
    IO(..), unIO, failIO, liftIO, bindIO, thenIO, returnIO, 
    unsafePerformIO, unsafeInterleaveIO,
    unsafeDupablePerformIO, unsafeDupableInterleaveIO,
    noDuplicate,

        -- To and from from ST
    stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        -- References
    IORef(..), newIORef, readIORef, writeIORef, 
    IOArray(..), newIOArray, readIOArray, writeIOArray, unsafeReadIOArray, unsafeWriteIOArray,
    MVar(..),

        -- Handles, file descriptors,
    FilePath,  
    Handle(..), Handle__(..), HandleType(..), IOMode(..), FD, 
    isReadableHandleType, isWritableHandleType, isReadWriteHandleType, showHandle,

        -- Buffers
    -- Buffer(..), RawBuffer, BufferState(..), 
    BufferList(..), BufferMode(..),
    --bufferIsWritable, bufferEmpty, bufferFull, 

        -- Exceptions
    Exception(..), ArithException(..), AsyncException(..), ArrayException(..),
    stackOverflow, heapOverflow, ioException, 
    IOError, IOException(..), IOErrorType(..), ioError, userError,
    ExitCode(..),
    throwIO, block, unblock, blocked, catchAny, catchException,
    evaluate,
    ErrorCall(..), AssertionFailed(..), assertError, untangle,
    BlockedOnDeadMVar(..), BlockedIndefinitely(..), Deadlock(..),
    blockedOnDeadMVar, blockedIndefinitely
  ) where

import GHC.Exception
import GHC.IO
import GHC.IO.Handle.Types
import GHC.IO.IOMode
import GHC.IO.Exception
import GHC.IOArray
import GHC.IORef
import GHC.MVar
import Foreign.C.Types

type FD = CInt
