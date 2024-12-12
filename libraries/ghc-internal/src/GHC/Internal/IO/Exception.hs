{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, MagicHash,
             ExistentialQuantification, ImplicitParams #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.Exception
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- IO-related Exception types and functions
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.Exception (
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

import GHC.Internal.Base
import GHC.Internal.Generics
import GHC.Internal.List
import GHC.Internal.IO
import GHC.Internal.Show
import GHC.Internal.Read
import GHC.Internal.Exception
import GHC.Internal.IO.Handle.Types
import GHC.Internal.Stack.Types (HasCallStack)
import {-# SOURCE #-} GHC.Internal.Stack ( withFrozenCallStack )
import GHC.Internal.Foreign.C.Types

import GHC.Internal.Data.Typeable ( cast )

-- ------------------------------------------------------------------------
-- Exception datatypes and operations

-- |The thread is blocked on an @MVar@, but there are no other references
-- to the @MVar@ so it can't ever continue.
data BlockedIndefinitelyOnMVar = BlockedIndefinitelyOnMVar

-- | @since base-4.1.0.0
instance Exception BlockedIndefinitelyOnMVar

-- | @since base-4.1.0.0
instance Show BlockedIndefinitelyOnMVar where
    showsPrec _ BlockedIndefinitelyOnMVar = showString "thread blocked indefinitely in an MVar operation"

blockedIndefinitelyOnMVar :: SomeException -- for the RTS
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

-----

-- |The thread is waiting to retry an STM transaction, but there are no
-- other references to any @TVar@s involved, so it can't ever continue.
data BlockedIndefinitelyOnSTM = BlockedIndefinitelyOnSTM

-- | @since base-4.1.0.0
instance Exception BlockedIndefinitelyOnSTM

-- | @since base-4.1.0.0
instance Show BlockedIndefinitelyOnSTM where
    showsPrec _ BlockedIndefinitelyOnSTM = showString "thread blocked indefinitely in an STM transaction"

blockedIndefinitelyOnSTM :: SomeException -- for the RTS
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

-----

-- |There are no runnable threads, so the program is deadlocked.
-- The @Deadlock@ exception is raised in the main thread only.
data Deadlock = Deadlock

-- | @since base-4.1.0.0
instance Exception Deadlock where
    displayException _ = "no threads to run:  infinite loop or deadlock?"

-- | @since base-4.1.0.0
instance Show Deadlock where
    showsPrec _ Deadlock = showString "<<deadlock>>"

-----

-- |This thread has exceeded its allocation limit.  See
-- 'GHC.Internal.System.Mem.setAllocationCounter' and
-- 'GHC.Internal.System.Mem.enableAllocationLimit'.
--
-- @since base-4.8.0.0
data AllocationLimitExceeded = AllocationLimitExceeded

-- | @since base-4.8.0.0
instance Exception AllocationLimitExceeded where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | @since base-4.7.1.0
instance Show AllocationLimitExceeded where
    showsPrec _ AllocationLimitExceeded =
      showString "allocation limit exceeded"

allocationLimitExceeded :: SomeException -- for the RTS
allocationLimitExceeded = toException AllocationLimitExceeded

-----

-- | Compaction found an object that cannot be compacted.  Functions
-- cannot be compacted, nor can mutable objects or pinned objects.
-- See 'GHC.Compact.compact'.
--
-- @since base-4.10.0.0
newtype CompactionFailed = CompactionFailed String

-- | @since base-4.10.0.0
instance Exception CompactionFailed where

-- | @since base-4.10.0.0
instance Show CompactionFailed where
    showsPrec _ (CompactionFailed why) =
      showString ("compaction failed: " ++ why)

cannotCompactFunction :: SomeException -- for the RTS
cannotCompactFunction =
  toException (CompactionFailed "cannot compact functions")

cannotCompactPinned :: SomeException -- for the RTS
cannotCompactPinned =
  toException (CompactionFailed "cannot compact pinned objects")

cannotCompactMutable :: SomeException -- for the RTS
cannotCompactMutable =
  toException (CompactionFailed "cannot compact mutable objects")

-----

-- |'assert' was applied to 'False'.
newtype AssertionFailed = AssertionFailed String

-- | @since base-4.1.0.0
instance Exception AssertionFailed

-- | @since base-4.1.0.0
instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

-----

-- |Superclass for asynchronous exceptions.
--
-- @since base-4.7.0.0
data SomeAsyncException = forall e . Exception e => SomeAsyncException e

-- | @since base-4.7.0.0
instance Show SomeAsyncException where
    showsPrec p (SomeAsyncException e) = showsPrec p e

-- | @since base-4.7.0.0
instance Exception SomeAsyncException

-- | @since base-4.7.0.0
asyncExceptionToException :: Exception e => e -> SomeException
asyncExceptionToException = toException . SomeAsyncException

-- | @since base-4.7.0.0
asyncExceptionFromException :: Exception e => SomeException -> Maybe e
asyncExceptionFromException x = do
    SomeAsyncException a <- fromException x
    cast a

-- | Asynchronous exceptions.
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
  deriving ( Eq  -- ^ @since base-4.2.0.0
           , Ord -- ^ @since base-4.2.0.0
           )

-- | @since base-4.7.0.0
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
  deriving ( Eq  -- ^ @since base-4.2.0.0
           , Ord -- ^ @since base-4.2.0.0
           )

-- | @since base-4.1.0.0
instance Exception ArrayException

-- for the RTS
stackOverflow, heapOverflow :: SomeException
stackOverflow = toException StackOverflow
heapOverflow  = toException HeapOverflow

-- | @since base-4.1.0.0
instance Show AsyncException where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
  showsPrec _ ThreadKilled    = showString "thread killed"
  showsPrec _ UserInterrupt   = showString "user interrupt"

-- | @since base-4.1.0.0
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
-- 'GHC.Internal.System.IO.fixIO'.
--
-- @since base-4.11.0.0
data FixIOException = FixIOException

-- | @since base-4.11.0.0
instance Exception FixIOException

-- | @since base-4.11.0.0
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

-- | @since base-4.1.0.0
instance Exception ExitCode

ioException     :: HasCallStack => IOException -> IO a
ioException err = withFrozenCallStack $ throwIO err

-- | Raise an 'IOError' in the 'IO' monad.
ioError         :: HasCallStack => IOError -> IO a
ioError         =  withFrozenCallStack ioException

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
     ioe_handle   :: Maybe Handle,   -- ^ the handle used by the action flagging
                                     --   the error.
     ioe_type     :: IOErrorType,    -- ^ what it was.
     ioe_location :: String,         -- ^ location.
     ioe_description :: String,      -- ^ error type specific information.
     ioe_errno    :: Maybe CInt,     -- ^ errno leading to this error, if any.
     ioe_filename :: Maybe FilePath  -- ^ filename the error is related to
                                     --   (some libraries may assume different encodings
                                     --   when constructing this field from e.g. 'ByteString'
                                     --   or other types)
   }

-- | @since base-4.1.0.0
instance Exception IOException

-- | @since base-4.1.0.0
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

-- | @since base-4.1.0.0
instance Eq IOErrorType where
   x == y = isTrue# (getTag x ==# getTag y)

-- | @since base-4.1.0.0
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

-- | @since base-4.1.0.0
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

assertError :: (?callStack :: CallStack) => Bool -> a -> a
-- See Note [Overview of assertions] in GHC.Tc.Gen.Head
assertError predicate v
  | predicate = v
  | otherwise = lazy $ unsafeDupablePerformIO $ do -- lazy: See Note [Strictness of assertError]
    throwIO (AssertionFailed "Assertion failed")

{- Note [Strictness of assertError]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is vital that Demand Analysis does not see `assertError p e` as strict in e.
#5561 details what happens otherwise, tested by libraries/base/tests/assert.hs:

  let e1 i = throw Overflow
  in assertError False (e1 5)

This should *not* throw the Overflow exception; rather it should throw an
AssertionError.
Hence we use GHC.Exts.lazy to make assertError appear lazy in e, so that it
is not called by-value.
(Note that the reason we need `lazy` in the first place is that error has a
bottoming result, which is strict in all free variables.)
The way we achieve this is a bit subtle; before #24625 we defined it as

  assertError p e | p         = lazy e
                  | otherwise = error "assertion"

but this means that in the following example (full code in T24625) we cannot
cancel away the allocation of `Just x` because of the intervening `lazy`:

  case assertError False (Just x) of Just y -> y
  ==> { simplify }
  case lazy (Just x) of Just y -> y

Instead, we put `lazy` in the otherwise branch, thus

  assertError p e | p         = e
                  | otherwise = lazy $ error "assertion"

The effect on #5561 is the same: since the otherwise branch appears lazy in e,
the overall demand on `e` must be lazy as well.
Furthermore, since there is no intervening `lazy` on the expected code path,
the Simplifier may perform case-of-case on e and simplify the `Just x` example
to `x`.
-}

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
