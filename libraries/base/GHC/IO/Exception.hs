{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, MagicHash,
             ExistentialQuantification, ImplicitParams #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Exception
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- IO-related Exception types and functions
--
-----------------------------------------------------------------------------

module GHC.IO.Exception (
  BlockedIndefinitelyOnMVar(..), blockedIndefinitelyOnMVar,
  BlockedIndefinitelyOnSTM(..), blockedIndefinitelyOnSTM,
  Deadlock(..),
  AllocationLimitExceeded(..), allocationLimitExceeded,
  AssertionFailed(..),
  CompactionFailed(..),
  cannotCompactFunction, cannotCompactPinned, cannotCompactMutable,

  SomeAsyncException(..),
  asyncExceptionToException, asyncExceptionFromException,
  AsyncException(..), stackOverflow, heapOverflow,

  ArrayException(..),
  ExitCode(..),
  FixIOException (..),

  ioException,
  ioError,
  IOError,
  IOException(..),
  IOErrorType(..),
  userError,
  assertError,
  unsupportedOperation,
  untangle,
 ) where

import GHC.Base
import GHC.Generics
import GHC.List
import GHC.IO
import GHC.Show
import GHC.Read
import GHC.Exception
import GHC.IO.Handle.Types
import GHC.OldList ( intercalate )
import {-# SOURCE #-} GHC.Stack.CCS
import Foreign.C.Types

import Data.Typeable ( cast )

-- ------------------------------------------------------------------------
-- Exception datatypes and operations

-- |The thread is blocked on an @MVar@, but there are no other references
-- to the @MVar@ so it can't ever continue.
data BlockedIndefinitelyOnMVar = BlockedIndefinitelyOnMVar

-- | @since 4.1.0.0
instance Exception BlockedIndefinitelyOnMVar

-- | @since 4.1.0.0
instance Show BlockedIndefinitelyOnMVar where
    showsPrec _ BlockedIndefinitelyOnMVar = showString "thread blocked indefinitely in an MVar operation"

blockedIndefinitelyOnMVar :: SomeExceptionWithLocation -- for the RTS
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

-----

-- |The thread is waiting to retry an STM transaction, but there are no
-- other references to any @TVar@s involved, so it can't ever continue.
data BlockedIndefinitelyOnSTM = BlockedIndefinitelyOnSTM

-- | @since 4.1.0.0
instance Exception BlockedIndefinitelyOnSTM

-- | @since 4.1.0.0
instance Show BlockedIndefinitelyOnSTM where
    showsPrec _ BlockedIndefinitelyOnSTM = showString "thread blocked indefinitely in an STM transaction"

blockedIndefinitelyOnSTM :: SomeExceptionWithLocation -- for the RTS
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

-----

-- |There are no runnable threads, so the program is deadlocked.
-- The @Deadlock@ exception is raised in the main thread only.
data Deadlock = Deadlock

-- | @since 4.1.0.0
instance Exception Deadlock

-- | @since 4.1.0.0
instance Show Deadlock where
    showsPrec _ Deadlock = showString "<<deadlock>>"

-----

-- |This thread has exceeded its allocation limit.  See
-- 'System.Mem.setAllocationCounter' and
-- 'System.Mem.enableAllocationLimit'.
--
-- @since 4.8.0.0
data AllocationLimitExceeded = AllocationLimitExceeded

-- | @since 4.8.0.0
instance Exception AllocationLimitExceeded where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | @since 4.7.1.0
instance Show AllocationLimitExceeded where
    showsPrec _ AllocationLimitExceeded =
      showString "allocation limit exceeded"

allocationLimitExceeded :: SomeExceptionWithLocation -- for the RTS
allocationLimitExceeded = toException AllocationLimitExceeded

-----

-- | Compaction found an object that cannot be compacted.  Functions
-- cannot be compacted, nor can mutable objects or pinned objects.
-- See 'GHC.Compact.compact'.
--
-- @since 4.10.0.0
newtype CompactionFailed = CompactionFailed String

-- | @since 4.10.0.0
instance Exception CompactionFailed where

-- | @since 4.10.0.0
instance Show CompactionFailed where
    showsPrec _ (CompactionFailed why) =
      showString ("compaction failed: " ++ why)

cannotCompactFunction :: SomeExceptionWithLocation -- for the RTS
cannotCompactFunction =
  toException (CompactionFailed "cannot compact functions")

cannotCompactPinned :: SomeExceptionWithLocation -- for the RTS
cannotCompactPinned =
  toException (CompactionFailed "cannot compact pinned objects")

cannotCompactMutable :: SomeExceptionWithLocation -- for the RTS
cannotCompactMutable =
  toException (CompactionFailed "cannot compact mutable objects")

-----

-- |'assert' was applied to 'False'.
newtype AssertionFailed = AssertionFailed String

-- | @since 4.1.0.0
instance Exception AssertionFailed

-- | @since 4.1.0.0
instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

-----

-- |Superclass for asynchronous exceptions.
--
-- @since 4.7.0.0
data SomeAsyncException = forall e . Exception e => SomeAsyncException e

-- | @since 4.7.0.0
instance Show SomeAsyncException where
    showsPrec p (SomeAsyncException e) = showsPrec p e

-- | @since 4.7.0.0
instance Exception SomeAsyncException

-- |@since 4.7.0.0
asyncExceptionToException :: Exception e => e -> SomeExceptionWithLocation
asyncExceptionToException = toException . SomeAsyncException

-- |@since 4.7.0.0
asyncExceptionFromException :: Exception e => SomeExceptionWithLocation -> Maybe e
asyncExceptionFromException x = do
    SomeAsyncException a <- fromException x
    cast a


-- |Asynchronous exceptions.
data AsyncException
  = StackOverflow
        -- ^The current thread\'s stack exceeded its limit.
        -- Since an exception has been raised, the thread\'s stack
        -- will certainly be below its limit again, but the
        -- programmer should take remedial action
        -- immediately.
  | HeapOverflow
        -- ^The program\'s heap is reaching its limit, and
        -- the program should take action to reduce the amount of
        -- live data it has. Notes:
        --
        --   * It is undefined which thread receives this exception.
        --     GHC currently throws this to the same thread that
        --     receives 'UserInterrupt', but this may change in the
        --     future.
        --
        --   * The GHC RTS currently can only recover from heap overflow
        --     if it detects that an explicit memory limit (set via RTS flags).
        --     has been exceeded.  Currently, failure to allocate memory from
        --     the operating system results in immediate termination of the
        --     program.
  | ThreadKilled
        -- ^This exception is raised by another thread
        -- calling 'Control.Concurrent.killThread', or by the system
        -- if it needs to terminate the thread for some
        -- reason.
  | UserInterrupt
        -- ^This exception is raised by default in the main thread of
        -- the program when the user requests to terminate the program
        -- via the usual mechanism(s) (e.g. Control-C in the console).
  deriving ( Eq  -- ^ @since 4.2.0.0
           , Ord -- ^ @since 4.2.0.0
           )

-- | @since 4.7.0.0
instance Exception AsyncException where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Exceptions generated by array operations
data ArrayException
  = IndexOutOfBounds    String
        -- ^An attempt was made to index an array outside
        -- its declared bounds.
  | UndefinedElement    String
        -- ^An attempt was made to evaluate an element of an
        -- array that had not been initialized.
  deriving ( Eq  -- ^ @since 4.2.0.0
           , Ord -- ^ @since 4.2.0.0
           )

-- | @since 4.1.0.0
instance Exception ArrayException

-- for the RTS
stackOverflow, heapOverflow :: SomeExceptionWithLocation
stackOverflow = toException StackOverflow
heapOverflow  = toException HeapOverflow

-- | @since 4.1.0.0
instance Show AsyncException where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
  showsPrec _ ThreadKilled    = showString "thread killed"
  showsPrec _ UserInterrupt   = showString "user interrupt"

-- | @since 4.1.0.0
instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s)
        = showString "array index out of range"
        . (if not (null s) then showString ": " . showString s
                           else id)
  showsPrec _ (UndefinedElement s)
        = showString "undefined array element"
        . (if not (null s) then showString ": " . showString s
                           else id)

-- | The exception thrown when an infinite cycle is detected in
-- 'System.IO.fixIO'.
--
-- @since 4.11.0.0
data FixIOException = FixIOException

-- | @since 4.11.0.0
instance Exception FixIOException

-- | @since 4.11.0.0
instance Show FixIOException where
  showsPrec _ FixIOException = showString "cyclic evaluation in fixIO"

-- -----------------------------------------------------------------------------
-- The ExitCode type

-- We need it here because it is used in ExitException in the
-- Exception datatype (above).

-- | Defines the exit codes that a program can return.
data ExitCode
  = ExitSuccess -- ^ indicates successful termination;
  | ExitFailure Int
                -- ^ indicates program failure with an exit code.
                -- The exact interpretation of the code is
                -- operating-system dependent.  In particular, some values
                -- may be prohibited (e.g. 0 on a POSIX-compliant system).
  deriving (Eq, Ord, Read, Show, Generic)

-- | @since 4.1.0.0
instance Exception ExitCode

ioException     :: IOException -> IO a
ioException err = throwIO err

-- | Raise an 'IOError' in the 'IO' monad.
ioError         :: IOError -> IO a
ioError         =  ioException

-- ---------------------------------------------------------------------------
-- IOError type

-- | The Haskell 2010 type for exceptions in the 'IO' monad.
-- Any I\/O operation may raise an 'IOError' instead of returning a result.
-- For a more general type of exception, including also those that arise
-- in pure code, see 'Control.Exception.Exception'.
--
-- In Haskell 2010, this is an opaque type.
type IOError = IOException

-- |Exceptions that occur in the @IO@ monad.
-- An @IOException@ records a more specific error type, a descriptive
-- string and maybe the handle that was used when the error was
-- flagged.
data IOException
 = IOError {
     ioe_handle   :: Maybe Handle,   -- the handle used by the action flagging
                                     -- the error.
     ioe_type     :: IOErrorType,    -- what it was.
     ioe_location :: String,         -- location.
     ioe_description :: String,      -- error type specific information.
     ioe_errno    :: Maybe CInt,     -- errno leading to this error, if any.
     ioe_filename :: Maybe FilePath  -- filename the error is related to.
   }

-- | @since 4.1.0.0
instance Exception IOException

-- | @since 4.1.0.0
instance Eq IOException where
  (IOError h1 e1 loc1 str1 en1 fn1) == (IOError h2 e2 loc2 str2 en2 fn2) =
    e1==e2 && str1==str2 && h1==h2 && loc1==loc2 && en1==en2 && fn1==fn2

-- | An abstract type that contains a value for each variant of 'IOError'.
data IOErrorType
  -- Haskell 2010:
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError
  -- GHC only:
  | UnsatisfiedConstraints
  | SystemError
  | ProtocolError
  | OtherError
  | InvalidArgument
  | InappropriateType
  | HardwareFault
  | UnsupportedOperation
  | TimeExpired
  | ResourceVanished
  | Interrupted

-- | @since 4.1.0.0
instance Eq IOErrorType where
   x == y = isTrue# (getTag x ==# getTag y)

-- | @since 4.1.0.0
instance Show IOErrorType where
  showsPrec _ e =
    showString $
    case e of
      AlreadyExists     -> "already exists"
      NoSuchThing       -> "does not exist"
      ResourceBusy      -> "resource busy"
      ResourceExhausted -> "resource exhausted"
      EOF               -> "end of file"
      IllegalOperation  -> "illegal operation"
      PermissionDenied  -> "permission denied"
      UserError         -> "user error"
      HardwareFault     -> "hardware fault"
      InappropriateType -> "inappropriate type"
      Interrupted       -> "interrupted"
      InvalidArgument   -> "invalid argument"
      OtherError        -> "failed"
      ProtocolError     -> "protocol error"
      ResourceVanished  -> "resource vanished"
      SystemError       -> "system error"
      TimeExpired       -> "timeout"
      UnsatisfiedConstraints -> "unsatisfied constraints" -- ultra-precise!
      UnsupportedOperation -> "unsupported operation"

-- | Construct an 'IOError' value with a string describing the error.
-- The 'fail' method of the 'IO' instance of the 'Monad' class raises a
-- 'userError', thus:
--
-- > instance Monad IO where
-- >   ...
-- >   fail s = ioError (userError s)
--
userError       :: String  -> IOError
userError str   =  IOError Nothing UserError "" str Nothing Nothing

-- ---------------------------------------------------------------------------
-- Showing IOErrors

-- | @since 4.1.0.0
instance Show IOException where
    showsPrec p (IOError hdl iot loc s _ fn) =
      (case fn of
         Nothing -> case hdl of
                        Nothing -> id
                        Just h  -> showsPrec p h . showString ": "
         Just name -> showString name . showString ": ") .
      (case loc of
         "" -> id
         _  -> showString loc . showString ": ") .
      showsPrec p iot .
      (case s of
         "" -> id
         _  -> showString " (" . showString s . showString ")")

-- Note the use of "lazy". This means that
--     assert False (throw e)
-- will throw the assertion failure rather than e. See trac #5561.
assertError :: (?callStack :: CallStack) => Bool -> a -> a
assertError predicate v
  | predicate = lazy v
  | otherwise = unsafeDupablePerformIO $ do
    ccsStack <- currentCallStack
    let
      implicitParamCallStack = prettyCallStackLines ?callStack
      ccsCallStack = showCCSStack ccsStack
      stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
    throwIO (AssertionFailed ("Assertion failed\n" ++ stack))

unsupportedOperation :: IOError
unsupportedOperation =
   (IOError Nothing UnsupportedOperation ""
        "Operation is not supported" Nothing Nothing)

{-
(untangle coded message) expects "coded" to be of the form
        "location|details"
It prints
        location message details
-}
untangle :: Addr# -> String -> String
untangle coded message
  =  location
  ++ ": "
  ++ message
  ++ details
  ++ "\n"
  where
    coded_str = unpackCStringUtf8# coded

    (location, details)
      = case (span not_bar coded_str) of { (loc, rest) ->
        case rest of
          ('|':det) -> (loc, ' ' : det)
          _         -> (loc, "")
        }
    not_bar c = c /= '|'
