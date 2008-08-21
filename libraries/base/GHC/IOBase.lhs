\begin{code}
{-# OPTIONS_GHC -XNoImplicitPrelude -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IOBase
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Definitions for the 'IO' monad and its friends.
--
-----------------------------------------------------------------------------

-- #hide
module GHC.IOBase(
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
    Buffer(..), RawBuffer, BufferState(..), BufferList(..), BufferMode(..),
    bufferIsWritable, bufferEmpty, bufferFull, 

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

import GHC.ST
import GHC.Arr  -- to derive Ix class
import GHC.Enum -- to derive Enum class
import GHC.STRef
import GHC.Base
--  import GHC.Num      -- To get fromInteger etc, needed because of -XNoImplicitPrelude
import Data.Maybe  ( Maybe(..) )
import GHC.Show
import GHC.List
import GHC.Read
import Foreign.C.Types (CInt)
import GHC.Exception

#ifndef __HADDOCK__
import {-# SOURCE #-} Data.Typeable     ( Typeable )
#endif

-- ---------------------------------------------------------------------------
-- The IO Monad

{-
The IO Monad is just an instance of the ST monad, where the state is
the real world.  We use the exception mechanism (in GHC.Exception) to
implement IO exceptions.

NOTE: The IO representation is deeply wired in to various parts of the
system.  The following list may or may not be exhaustive:

Compiler  - types of various primitives in PrimOp.lhs

RTS       - forceIO (StgMiscClosures.hc)
          - catchzh_fast, (un)?blockAsyncExceptionszh_fast, raisezh_fast 
            (Exceptions.hc)
          - raiseAsync (Schedule.c)

Prelude   - GHC.IOBase.lhs, and several other places including
            GHC.Exception.lhs.

Libraries - parts of hslibs/lang.

--SDM
-}

{-|
A value of type @'IO' a@ is a computation which, when performed,
does some I\/O before returning a value of type @a@.  

There is really only one way to \"perform\" an I\/O action: bind it to
@Main.main@ in your program.  When your program is run, the I\/O will
be performed.  It isn't possible to perform I\/O from an arbitrary
function, unless that function is itself in the 'IO' monad and called
at some point, directly or indirectly, from @Main.main@.

'IO' is a monad, so 'IO' actions can be combined using either the do-notation
or the '>>' and '>>=' operations from the 'Monad' class.
-}
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

instance  Functor IO where
   fmap f x = x >>= (return . f)

instance  Monad IO  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      =  m >>= \ _ -> k
    return x    = returnIO x

    m >>= k     = bindIO m k
    fail s      = failIO s

failIO :: String -> IO a
failIO s = ioError (userError s)

liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO ( \ s ->
  case m s of 
    (# new_s, a #) -> unIO (k a) new_s
  )

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO ( \ s ->
  case m s of 
    (# new_s, _ #) -> unIO k new_s
  )

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | A monad transformer embedding strict state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce# io) s

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce# m)

-- ---------------------------------------------------------------------------
-- Unsafe IO operations

{-|
This is the \"back door\" into the 'IO' monad, allowing
'IO' computation to be performed at any time.  For
this to be safe, the 'IO' computation should be
free of side effects and independent of its environment.

If the I\/O computation wrapped in 'unsafePerformIO'
performs side effects, then the relative order in which those side
effects take place (relative to the main I\/O trunk, or other calls to
'unsafePerformIO') is indeterminate.  You have to be careful when 
writing and compiling modules that use 'unsafePerformIO':

  * Use @{\-\# NOINLINE foo \#-\}@ as a pragma on any function @foo@
        that calls 'unsafePerformIO'.  If the call is inlined,
        the I\/O may be performed more than once.

  * Use the compiler flag @-fno-cse@ to prevent common sub-expression
        elimination being performed on the module, which might combine
        two side effects that were meant to be separate.  A good example
        is using multiple global variables (like @test@ in the example below).

  * Make sure that the either you switch off let-floating, or that the 
        call to 'unsafePerformIO' cannot float outside a lambda.  For example, 
        if you say:
        @
           f x = unsafePerformIO (newIORef [])
        @
        you may get only one reference cell shared between all calls to @f@.
        Better would be
        @
           f x = unsafePerformIO (newIORef [x])
        @
        because now it can't float outside the lambda.

It is less well known that
'unsafePerformIO' is not type safe.  For example:

>     test :: IORef [a]
>     test = unsafePerformIO $ newIORef []
>     
>     main = do
>             writeIORef test [42]
>             bang <- readIORef test
>             print (bang :: [Char])

This program will core dump.  This problem with polymorphic references
is well known in the ML community, and does not arise with normal
monadic use of references.  There is no easy way to make it impossible
once you use 'unsafePerformIO'.  Indeed, it is
possible to write @coerce :: a -> b@ with the
help of 'unsafePerformIO'.  So be careful!
-}
unsafePerformIO :: IO a -> a
unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)

{-| 
This version of 'unsafePerformIO' is slightly more efficient,
because it omits the check that the IO is only being performed by a
single thread.  Hence, when you write 'unsafeDupablePerformIO',
there is a possibility that the IO action may be performed multiple
times (on a multiprocessor), and you should therefore ensure that
it gives the same results each time.
-}
{-# NOINLINE unsafeDupablePerformIO #-}
unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = lazy (case m realWorld# of (# _, r #) -> r)

-- Why do we NOINLINE unsafeDupablePerformIO?  See the comment with
-- GHC.ST.runST.  Essentially the issue is that the IO computation
-- inside unsafePerformIO must be atomic: it must either all run, or
-- not at all.  If we let the compiler see the application of the IO
-- to realWorld#, it might float out part of the IO.

-- Why is there a call to 'lazy' in unsafeDupablePerformIO?
-- If we don't have it, the demand analyser discovers the following strictness
-- for unsafeDupablePerformIO:  C(U(AV))
-- But then consider
--      unsafeDupablePerformIO (\s -> let r = f x in 
--                             case writeIORef v r s of (# s1, _ #) ->
--                             (# s1, r #)
-- The strictness analyser will find that the binding for r is strict,
-- (becuase of uPIO's strictness sig), and so it'll evaluate it before 
-- doing the writeIORef.  This actually makes tests/lib/should_run/memo002
-- get a deadlock!  
--
-- Solution: don't expose the strictness of unsafeDupablePerformIO,
--           by hiding it with 'lazy'

{-|
'unsafeInterleaveIO' allows 'IO' computation to be deferred lazily.
When passed a value of type @IO a@, the 'IO' will only be performed
when the value of the @a@ is demanded.  This is used to implement lazy
file reading, see 'System.IO.hGetContents'.
-}
{-# INLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = unsafeDupableInterleaveIO (noDuplicate >> m)

-- We believe that INLINE on unsafeInterleaveIO is safe, because the
-- state from this IO thread is passed explicitly to the interleaved
-- IO, so it cannot be floated out and shared.

{-# INLINE unsafeDupableInterleaveIO #-}
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO (IO m)
  = IO ( \ s -> let
                   r = case m s of (# _, res #) -> res
                in
                (# s, r #))

{-| 
Ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.
-}
noDuplicate :: IO ()
noDuplicate = IO $ \s -> case noDuplicate# s of s' -> (# s', () #)

-- ---------------------------------------------------------------------------
-- Handle type

data MVar a = MVar (MVar# RealWorld a)
{- ^
An 'MVar' (pronounced \"em-var\") is a synchronising variable, used
for communication between concurrent threads.  It can be thought of
as a a box, which may be empty or full.
-}

-- pull in Eq (Mvar a) too, to avoid GHC.Conc being an orphan-instance module
instance Eq (MVar a) where
        (MVar mvar1#) == (MVar mvar2#) = sameMVar# mvar1# mvar2#

--  A Handle is represented by (a reference to) a record 
--  containing the state of the I/O port/device. We record
--  the following pieces of info:

--    * type (read,write,closed etc.)
--    * the underlying file descriptor
--    * buffering mode 
--    * buffer, and spare buffers
--    * user-friendly name (usually the
--      FilePath used when IO.openFile was called)

-- Note: when a Handle is garbage collected, we want to flush its buffer
-- and close the OS file handle, so as to free up a (precious) resource.

-- | Haskell defines operations to read and write characters from and to files,
-- represented by values of type @Handle@.  Each value of this type is a
-- /handle/: a record used by the Haskell run-time system to /manage/ I\/O
-- with file system objects.  A handle has at least the following properties:
-- 
--  * whether it manages input or output or both;
--
--  * whether it is /open/, /closed/ or /semi-closed/;
--
--  * whether the object is seekable;
--
--  * whether buffering is disabled, or enabled on a line or block basis;
--
--  * a buffer (whose length may be zero).
--
-- Most handles will also have a current I\/O position indicating where the next
-- input or output operation will occur.  A handle is /readable/ if it
-- manages only input or both input and output; likewise, it is /writable/ if
-- it manages only output or both input and output.  A handle is /open/ when
-- first allocated.
-- Once it is closed it can no longer be used for either input or output,
-- though an implementation cannot re-use its storage while references
-- remain to it.  Handles are in the 'Show' and 'Eq' classes.  The string
-- produced by showing a handle is system dependent; it should include
-- enough information to identify the handle for debugging.  A handle is
-- equal according to '==' only to itself; no attempt
-- is made to compare the internal state of different handles for equality.
--
-- GHC note: a 'Handle' will be automatically closed when the garbage
-- collector detects that it has become unreferenced by the program.
-- However, relying on this behaviour is not generally recommended:
-- the garbage collector is unpredictable.  If possible, use explicit
-- an explicit 'hClose' to close 'Handle's when they are no longer
-- required.  GHC does not currently attempt to free up file
-- descriptors when they have run out, it is your responsibility to
-- ensure that this doesn't happen.

data Handle 
  = FileHandle                          -- A normal handle to a file
        FilePath                        -- the file (invariant)
        !(MVar Handle__)

  | DuplexHandle                        -- A handle to a read/write stream
        FilePath                        -- file for a FIFO, otherwise some
                                        --   descriptive string.
        !(MVar Handle__)                -- The read side
        !(MVar Handle__)                -- The write side

-- NOTES:
--    * A 'FileHandle' is seekable.  A 'DuplexHandle' may or may not be
--      seekable.

instance Eq Handle where
 (FileHandle _ h1)     == (FileHandle _ h2)     = h1 == h2
 (DuplexHandle _ h1 _) == (DuplexHandle _ h2 _) = h1 == h2
 _ == _ = False 

type FD = CInt

data Handle__
  = Handle__ {
      haFD          :: !FD,                  -- file descriptor
      haType        :: HandleType,           -- type (read/write/append etc.)
      haIsBin       :: Bool,                 -- binary mode?
      haIsStream    :: Bool,                 -- Windows : is this a socket?
                                             -- Unix    : is O_NONBLOCK set?
      haBufferMode  :: BufferMode,           -- buffer contains read/write data?
      haBuffer      :: !(IORef Buffer),      -- the current buffer
      haBuffers     :: !(IORef BufferList),  -- spare buffers
      haOtherSide   :: Maybe (MVar Handle__) -- ptr to the write side of a 
                                             -- duplex handle.
    }

-- ---------------------------------------------------------------------------
-- Buffers

-- The buffer is represented by a mutable variable containing a
-- record, where the record contains the raw buffer and the start/end
-- points of the filled portion.  We use a mutable variable so that
-- the common operation of writing (or reading) some data from (to)
-- the buffer doesn't need to modify, and hence copy, the handle
-- itself, it just updates the buffer.  

-- There will be some allocation involved in a simple hPutChar in
-- order to create the new Buffer structure (below), but this is
-- relatively small, and this only has to be done once per write
-- operation.

-- The buffer contains its size - we could also get the size by
-- calling sizeOfMutableByteArray# on the raw buffer, but that tends
-- to be rounded up to the nearest Word.

type RawBuffer = MutableByteArray# RealWorld

-- INVARIANTS on a Buffer:
--
--   * A handle *always* has a buffer, even if it is only 1 character long
--     (an unbuffered handle needs a 1 character buffer in order to support
--      hLookAhead and hIsEOF).
--   * r <= w
--   * if r == w, then r == 0 && w == 0
--   * if state == WriteBuffer, then r == 0
--   * a write buffer is never full.  If an operation
--     fills up the buffer, it will always flush it before 
--     returning.
--   * a read buffer may be full as a result of hLookAhead.  In normal
--     operation, a read buffer always has at least one character of space.

data Buffer 
  = Buffer {
        bufBuf   :: RawBuffer,
        bufRPtr  :: !Int,
        bufWPtr  :: !Int,
        bufSize  :: !Int,
        bufState :: BufferState
  }

data BufferState = ReadBuffer | WriteBuffer deriving (Eq)

-- we keep a few spare buffers around in a handle to avoid allocating
-- a new one for each hPutStr.  These buffers are *guaranteed* to be the
-- same size as the main buffer.
data BufferList 
  = BufferListNil 
  | BufferListCons RawBuffer BufferList


bufferIsWritable :: Buffer -> Bool
bufferIsWritable Buffer{ bufState=WriteBuffer } = True
bufferIsWritable _other = False

bufferEmpty :: Buffer -> Bool
bufferEmpty Buffer{ bufRPtr=r, bufWPtr=w } = r == w

-- only makes sense for a write buffer
bufferFull :: Buffer -> Bool
bufferFull b@Buffer{ bufWPtr=w } = w >= bufSize b

--  Internally, we classify handles as being one
--  of the following:

data HandleType
 = ClosedHandle
 | SemiClosedHandle
 | ReadHandle
 | WriteHandle
 | AppendHandle
 | ReadWriteHandle

isReadableHandleType :: HandleType -> Bool
isReadableHandleType ReadHandle         = True
isReadableHandleType ReadWriteHandle    = True
isReadableHandleType _                  = False

isWritableHandleType :: HandleType -> Bool
isWritableHandleType AppendHandle    = True
isWritableHandleType WriteHandle     = True
isWritableHandleType ReadWriteHandle = True
isWritableHandleType _               = False

isReadWriteHandleType :: HandleType -> Bool
isReadWriteHandleType ReadWriteHandle{} = True
isReadWriteHandleType _                 = False

-- | File and directory names are values of type 'String', whose precise
-- meaning is operating system dependent. Files can be opened, yielding a
-- handle which can then be used to operate on the contents of that file.

type FilePath = String

-- ---------------------------------------------------------------------------
-- Buffering modes

-- | Three kinds of buffering are supported: line-buffering, 
-- block-buffering or no-buffering.  These modes have the following
-- effects. For output, items are written out, or /flushed/,
-- from the internal buffer according to the buffer mode:
--
--  * /line-buffering/: the entire output buffer is flushed
--    whenever a newline is output, the buffer overflows, 
--    a 'System.IO.hFlush' is issued, or the handle is closed.
--
--  * /block-buffering/: the entire buffer is written out whenever it
--    overflows, a 'System.IO.hFlush' is issued, or the handle is closed.
--
--  * /no-buffering/: output is written immediately, and never stored
--    in the buffer.
--
-- An implementation is free to flush the buffer more frequently,
-- but not less frequently, than specified above.
-- The output buffer is emptied as soon as it has been written out.
--
-- Similarly, input occurs according to the buffer mode for the handle:
--
--  * /line-buffering/: when the buffer for the handle is not empty,
--    the next item is obtained from the buffer; otherwise, when the
--    buffer is empty, characters up to and including the next newline
--    character are read into the buffer.  No characters are available
--    until the newline character is available or the buffer is full.
--
--  * /block-buffering/: when the buffer for the handle becomes empty,
--    the next block of data is read into the buffer.
--
--  * /no-buffering/: the next input item is read and returned.
--    The 'System.IO.hLookAhead' operation implies that even a no-buffered
--    handle may require a one-character buffer.
--
-- The default buffering mode when a handle is opened is
-- implementation-dependent and may depend on the file system object
-- which is attached to that handle.
-- For most implementations, physical files will normally be block-buffered 
-- and terminals will normally be line-buffered.

data BufferMode  
 = NoBuffering  -- ^ buffering is disabled if possible.
 | LineBuffering
                -- ^ line-buffering should be enabled if possible.
 | BlockBuffering (Maybe Int)
                -- ^ block-buffering should be enabled if possible.
                -- The size of the buffer is @n@ items if the argument
                -- is 'Just' @n@ and is otherwise implementation-dependent.
   deriving (Eq, Ord, Read, Show)

-- ---------------------------------------------------------------------------
-- IORefs

-- |A mutable variable in the 'IO' monad
newtype IORef a = IORef (STRef RealWorld a)

-- explicit instance because Haddock can't figure out a derived one
instance Eq (IORef a) where
  IORef x == IORef y = x == y

-- |Build a new 'IORef'
newIORef    :: a -> IO (IORef a)
newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)

-- |Read the value of an 'IORef'
readIORef   :: IORef a -> IO a
readIORef  (IORef var) = stToIO (readSTRef var)

-- |Write a new value into an 'IORef'
writeIORef  :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeSTRef var v)

-- ---------------------------------------------------------------------------
-- | An 'IOArray' is a mutable, boxed, non-strict array in the 'IO' monad.  
-- The type arguments are as follows:
--
--  * @i@: the index type of the array (should be an instance of 'Ix')
--
--  * @e@: the element type of the array.
--
-- 

newtype IOArray i e = IOArray (STArray RealWorld i e)

-- explicit instance because Haddock can't figure out a derived one
instance Eq (IOArray i e) where
  IOArray x == IOArray y = x == y

-- |Build a new 'IOArray'
newIOArray :: Ix i => (i,i) -> e -> IO (IOArray i e)
{-# INLINE newIOArray #-}
newIOArray lu initial  = stToIO $ do {marr <- newSTArray lu initial; return (IOArray marr)}

-- | Read a value from an 'IOArray'
unsafeReadIOArray  :: Ix i => IOArray i e -> Int -> IO e
{-# INLINE unsafeReadIOArray #-}
unsafeReadIOArray (IOArray marr) i = stToIO (unsafeReadSTArray marr i)

-- | Write a new value into an 'IOArray'
unsafeWriteIOArray :: Ix i => IOArray i e -> Int -> e -> IO ()
{-# INLINE unsafeWriteIOArray #-}
unsafeWriteIOArray (IOArray marr) i e = stToIO (unsafeWriteSTArray marr i e)

-- | Read a value from an 'IOArray'
readIOArray  :: Ix i => IOArray i e -> i -> IO e
readIOArray (IOArray marr) i = stToIO (readSTArray marr i)

-- | Write a new value into an 'IOArray'
writeIOArray :: Ix i => IOArray i e -> i -> e -> IO ()
writeIOArray (IOArray marr) i e = stToIO (writeSTArray marr i e)


-- ---------------------------------------------------------------------------
-- Show instance for Handles

-- handle types are 'show'n when printing error msgs, so
-- we provide a more user-friendly Show instance for it
-- than the derived one.

instance Show HandleType where
  showsPrec _ t =
    case t of
      ClosedHandle      -> showString "closed"
      SemiClosedHandle  -> showString "semi-closed"
      ReadHandle        -> showString "readable"
      WriteHandle       -> showString "writable"
      AppendHandle      -> showString "writable (append)"
      ReadWriteHandle   -> showString "read-writable"

instance Show Handle where 
  showsPrec _ (FileHandle   file _)   = showHandle file
  showsPrec _ (DuplexHandle file _ _) = showHandle file

showHandle :: FilePath -> String -> String
showHandle file = showString "{handle: " . showString file . showString "}"

-- ------------------------------------------------------------------------
-- Exception datatypes and operations

data BlockedOnDeadMVar = BlockedOnDeadMVar
    deriving Typeable

instance Exception BlockedOnDeadMVar

instance Show BlockedOnDeadMVar where
    showsPrec _ BlockedOnDeadMVar = showString "thread blocked indefinitely"

blockedOnDeadMVar :: SomeException -- for the RTS
blockedOnDeadMVar = toException BlockedOnDeadMVar

-----

data BlockedIndefinitely = BlockedIndefinitely
    deriving Typeable

instance Exception BlockedIndefinitely

instance Show BlockedIndefinitely where
    showsPrec _ BlockedIndefinitely = showString "thread blocked indefinitely"

blockedIndefinitely :: SomeException -- for the RTS
blockedIndefinitely = toException BlockedIndefinitely

-----

data Deadlock = Deadlock
    deriving Typeable

instance Exception Deadlock

instance Show Deadlock where
    showsPrec _ Deadlock = showString "<<deadlock>>"

-----

data AssertionFailed = AssertionFailed String
    deriving Typeable

instance Exception AssertionFailed

instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

-----

-- |Asynchronous exceptions
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
        --      * It is undefined which thread receives this exception.
        --
        --      * GHC currently does not throw 'HeapOverflow' exceptions.
  | ThreadKilled
        -- ^This exception is raised by another thread
        -- calling 'Control.Concurrent.killThread', or by the system
        -- if it needs to terminate the thread for some
        -- reason.
  | UserInterrupt
        -- ^This exception is raised by default in the main thread of
        -- the program when the user requests to terminate the program
        -- via the usual mechanism(s) (e.g. Control-C in the console).
  deriving (Eq, Ord, Typeable)

instance Exception AsyncException

-- | Exceptions generated by array operations
data ArrayException
  = IndexOutOfBounds    String
        -- ^An attempt was made to index an array outside
        -- its declared bounds.
  | UndefinedElement    String
        -- ^An attempt was made to evaluate an element of an
        -- array that had not been initialized.
  deriving (Eq, Ord, Typeable)

instance Exception ArrayException

stackOverflow, heapOverflow :: SomeException -- for the RTS
stackOverflow = toException StackOverflow
heapOverflow  = toException HeapOverflow

instance Show AsyncException where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
  showsPrec _ ThreadKilled    = showString "thread killed"
  showsPrec _ UserInterrupt   = showString "user interrupt"

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s)
        = showString "array index out of range"
        . (if not (null s) then showString ": " . showString s
                           else id)
  showsPrec _ (UndefinedElement s)
        = showString "undefined array element"
        . (if not (null s) then showString ": " . showString s
                           else id)

-- -----------------------------------------------------------------------------
-- The ExitCode type

-- We need it here because it is used in ExitException in the
-- Exception datatype (above).

data ExitCode
  = ExitSuccess -- ^ indicates successful termination;
  | ExitFailure Int
                -- ^ indicates program failure with an exit code.
                -- The exact interpretation of the code is
                -- operating-system dependent.  In particular, some values
                -- may be prohibited (e.g. 0 on a POSIX-compliant system).
  deriving (Eq, Ord, Read, Show, Typeable)

instance Exception ExitCode

ioException     :: IOException -> IO a
ioException err = throwIO err

-- | Raise an 'IOError' in the 'IO' monad.
ioError         :: IOError -> IO a 
ioError         =  ioException

-- ---------------------------------------------------------------------------
-- IOError type

-- | The Haskell 98 type for exceptions in the 'IO' monad.
-- Any I\/O operation may raise an 'IOError' instead of returning a result.
-- For a more general type of exception, including also those that arise
-- in pure code, see 'Control.Exception.Exception'.
--
-- In Haskell 98, this is an opaque type.
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
     ioe_filename :: Maybe FilePath  -- filename the error is related to.
   }
    deriving Typeable

instance Exception IOException

instance Eq IOException where
  (IOError h1 e1 loc1 str1 fn1) == (IOError h2 e2 loc2 str2 fn2) = 
    e1==e2 && str1==str2 && h1==h2 && loc1==loc2 && fn1==fn2

-- | An abstract type that contains a value for each variant of 'IOError'.
data IOErrorType
  -- Haskell 98:
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

instance Eq IOErrorType where
   x == y = getTag x ==# getTag y
 
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
      UnsatisfiedConstraints -> "unsatisified constraints" -- ultra-precise!
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
userError str   =  IOError Nothing UserError "" str Nothing

-- ---------------------------------------------------------------------------
-- Showing IOErrors

instance Show IOException where
    showsPrec p (IOError hdl iot loc s fn) =
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

-- -----------------------------------------------------------------------------
-- IOMode type

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Enum, Read, Show)
\end{code}

%*********************************************************
%*                                                      *
\subsection{Primitive catch and throwIO}
%*                                                      *
%*********************************************************

catchException used to handle the passing around of the state to the
action and the handler.  This turned out to be a bad idea - it meant
that we had to wrap both arguments in thunks so they could be entered
as normal (remember IO returns an unboxed pair...).

Now catch# has type

    catch# :: IO a -> (b -> IO a) -> IO a

(well almost; the compiler doesn't know about the IO newtype so we
have to work around that in the definition of catchException below).

\begin{code}
catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raise# e

catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny (IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)

-- | A variant of 'throw' that can be used within the 'IO' monad.
--
-- Although 'throwIO' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e   `seq` x  ===> throw e
-- > throwIO e `seq` x  ===> x
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throwIO' will only cause
-- an exception to be raised when it is used within the 'IO' monad.
-- The 'throwIO' variant should be used in preference to 'throw' to
-- raise an exception within the 'IO' monad because it guarantees
-- ordering with respect to other 'IO' operations, whereas 'throw'
-- does not.
throwIO :: Exception e => e -> IO a
throwIO e = IO (raiseIO# (toException e))
\end{code}


%*********************************************************
%*                                                      *
\subsection{Controlling asynchronous exception delivery}
%*                                                      *
%*********************************************************

\begin{code}
-- | Applying 'block' to a computation will
-- execute that computation with asynchronous exceptions
-- /blocked/.  That is, any thread which
-- attempts to raise an exception in the current thread with 'Control.Exception.throwTo' will be
-- blocked until asynchronous exceptions are enabled again.  There\'s
-- no need to worry about re-enabling asynchronous exceptions; that is
-- done automatically on exiting the scope of
-- 'block'.
--
-- Threads created by 'Control.Concurrent.forkIO' inherit the blocked
-- state from the parent; that is, to start a thread in blocked mode,
-- use @block $ forkIO ...@.  This is particularly useful if you need to
-- establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.
block :: IO a -> IO a

-- | To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblock :: IO a -> IO a

block (IO io) = IO $ blockAsyncExceptions# io
unblock (IO io) = IO $ unblockAsyncExceptions# io

-- | returns True if asynchronous exceptions are blocked in the
-- current thread.
blocked :: IO Bool
blocked = IO $ \s -> case asyncExceptionsBlocked# s of
                        (# s', i #) -> (# s', i /=# 0# #)
\end{code}

\begin{code}
-- | Forces its argument to be evaluated when the resultant 'IO' action
-- is executed.  It can be used to order evaluation with respect to
-- other 'IO' operations; its semantics are given by
--
-- >   evaluate x `seq` y    ==>  y
-- >   evaluate x `catch` f  ==>  (return $! x) `catch` f
-- >   evaluate x >>= f      ==>  (return $! x) >>= f
--
-- /Note:/ the first equation implies that @(evaluate x)@ is /not/ the
-- same as @(return $! x)@.  A correct definition is
--
-- >   evaluate x = (return $! x) >>= return
--
evaluate :: a -> IO a
evaluate a = IO $ \s -> case a `seq` () of () -> (# s, a #)
        -- NB. can't write
        --      a `seq` (# s, a #)
        -- because we can't have an unboxed tuple as a function argument
\end{code}

\begin{code}
assertError :: Addr# -> Bool -> a -> a
assertError str predicate v
  | predicate = v
  | otherwise = throw (AssertionFailed (untangle str "Assertion failed"))

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
\end{code}

