% ------------------------------------------------------------------------------
% $Id: IOBase.lhs,v 1.4 2001/09/13 15:34:17 simonmar Exp $
% 
% (c) The University of Glasgow, 1994-2001
%

% Definitions for the @IO@ monad and its friends.  Everything is exported
% concretely; the @IO@ module itself exports abstractly.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
#include "config.h"

module GHC.IOBase where

import GHC.ST
import GHC.STRef
import GHC.Base
import GHC.Num	-- To get fromInteger etc, needed because of -fno-implicit-prelude
import Data.Maybe  ( Maybe(..) )
import GHC.Show
import GHC.List
import GHC.Read
import GHC.Dynamic

-- ---------------------------------------------------------------------------
-- The IO Monad

{-
The IO Monad is just an instance of the ST monad, where the state is
the real world.  We use the exception mechanism (in GHC.Exception) to
implement IO exceptions.

NOTE: The IO representation is deeply wired in to various parts of the
system.  The following list may or may not be exhaustive:

Compiler  - types of various primitives in PrimOp.lhs

RTS 	  - forceIO (StgMiscClosures.hc)
	  - catchzh_fast, (un)?blockAsyncExceptionszh_fast, raisezh_fast 
	    (Exceptions.hc)
	  - raiseAsync (Schedule.c)

Prelude   - GHC.IOBase.lhs, and several other places including
	    GHC.Exception.lhs.

Libraries - parts of hslibs/lang.

--SDM
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
    return x	= returnIO x

    m >>= k     = bindIO m k
    fail s	= failIO s

failIO :: String -> IO a
failIO s = ioError (userError s)

liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO ( \ s ->
  case m s of 
    (# new_s, a #) -> unIO (k a) new_s
  )

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

--stToIO        :: (forall s. ST s a) -> IO a
stToIO	      :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

ioToST	      :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- ---------------------------------------------------------------------------
-- Unsafe IO operations

{-# NOINLINE unsafePerformIO #-}
unsafePerformIO	:: IO a -> a
unsafePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

{-# NOINLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO (IO m)
  = IO ( \ s -> let
		   r = case m s of (# _, res #) -> res
		in
		(# s, r #))

-- ---------------------------------------------------------------------------
-- Handle type

data MVar a = MVar (MVar# RealWorld a)

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
--	FilePath used when IO.openFile was called)

-- Note: when a Handle is garbage collected, we want to flush its buffer
-- and close the OS file handle, so as to free up a (precious) resource.

data Handle 
  = FileHandle				-- A normal handle to a file
	!(MVar Handle__)

  | DuplexHandle			-- A handle to a read/write stream
	!(MVar Handle__)		-- The read side
	!(MVar Handle__)		-- The write side

-- NOTES:
--    * A 'FileHandle' is seekable.  A 'DuplexHandle' may or may not be
--      seekable.

instance Eq Handle where
 (FileHandle h1)     == (FileHandle h2)     = h1 == h2
 (DuplexHandle h1 _) == (DuplexHandle h2 _) = h1 == h2
 _ == _ = False 

type FD = Int -- XXX ToDo: should be CInt

data Handle__
  = Handle__ {
      haFD	    :: !FD,
      haType        :: HandleType,
      haIsBin	    :: Bool,
      haBufferMode  :: BufferMode,
      haFilePath    :: FilePath,
      haBuffer	    :: !(IORef Buffer),
      haBuffers     :: !(IORef BufferList)
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
 | ReadSideHandle  !(MVar Handle__)	-- read side of a duplex handle

isReadableHandleType ReadHandle         = True
isReadableHandleType ReadWriteHandle    = True
isReadableHandleType (ReadSideHandle _) = True
isReadableHandleType _	       	        = False

isWritableHandleType AppendHandle    = True
isWritableHandleType WriteHandle     = True
isWritableHandleType ReadWriteHandle = True
isWritableHandleType _	       	     = False

-- File names are specified using @FilePath@, a OS-dependent
-- string that (hopefully, I guess) maps to an accessible file/object.

type FilePath = String

-- ---------------------------------------------------------------------------
-- Buffering modes

-- Three kinds of buffering are supported: line-buffering, 
-- block-buffering or no-buffering.  These modes have the following
-- effects. For output, items are written out from the internal
-- buffer according to the buffer mode:
--
-- * line-buffering  the entire output buffer is written
--   out whenever a newline is output, the output buffer overflows, 
--   a flush is issued, or the handle is closed.
--
-- * block-buffering the entire output buffer is written out whenever 
--   it overflows, a flush is issued, or the handle
--   is closed.
--
-- * no-buffering output is written immediately, and never stored
--   in the output buffer.
--
-- The output buffer is emptied as soon as it has been written out.

-- Similarly, input occurs according to the buffer mode for handle {\em hdl}.

-- * line-buffering when the input buffer for the handle is not empty,
--   the next item is obtained from the buffer;
--   otherwise, when the input buffer is empty,
--   characters up to and including the next newline
--   character are read into the buffer.  No characters
--   are available until the newline character is
--   available.
--
-- * block-buffering when the input buffer for the handle becomes empty,
--   the next block of data is read into this buffer.
--
-- * no-buffering the next input item is read and returned.

-- For most implementations, physical files will normally be block-buffered 
-- and terminals will normally be line-buffered. (the IO interface provides
-- operations for changing the default buffering of a handle tho.)

data BufferMode  
 = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
   deriving (Eq, Ord, Read, Show)

-- ---------------------------------------------------------------------------
-- IORefs

newtype IORef a = IORef (STRef RealWorld a) deriving Eq

newIORef    :: a -> IO (IORef a)
newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)

readIORef   :: IORef a -> IO a
readIORef  (IORef var) = stToIO (readSTRef var)

writeIORef  :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeSTRef var v)

-- ---------------------------------------------------------------------------
-- Show instance for Handles

-- handle types are 'show'n when printing error msgs, so
-- we provide a more user-friendly Show instance for it
-- than the derived one.

instance Show HandleType where
  showsPrec p t =
    case t of
      ClosedHandle      -> showString "closed"
      SemiClosedHandle  -> showString "semi-closed"
      ReadHandle        -> showString "readable"
      WriteHandle       -> showString "writable"
      AppendHandle      -> showString "writable (append)"
      ReadWriteHandle   -> showString "read-writable"
      ReadSideHandle _  -> showString "read-writable (duplex)"

instance Show Handle where 
  showsPrec p (FileHandle   h)   = showHandle p h
  showsPrec p (DuplexHandle h _) = showHandle p h
   
showHandle p h =
    let
     -- (Big) SIGH: unfolded defn of takeMVar to avoid
     -- an (oh-so) unfortunate module loop with GHC.Conc.
     hdl_ = unsafePerformIO (IO $ \ s# ->
	     case h                 of { MVar h# ->
	     case takeMVar# h# s#   of { (# s2# , r #) -> 
	     case putMVar# h# r s2# of { s3# ->
	     (# s3#, r #) }}})
    in
    showChar '{' . 
    showHdl (haType hdl_) 
	    (showString "loc=" . showString (haFilePath hdl_) . showChar ',' .
	     showString "type=" . showsPrec p (haType hdl_) . showChar ',' .
	     showString "binary=" . showsPrec p (haIsBin hdl_) . showChar ',' .
	     showString "buffering=" . showBufMode (unsafePerformIO (readIORef (haBuffer hdl_))) (haBufferMode hdl_) . showString "}" )
   where
    showHdl :: HandleType -> ShowS -> ShowS
    showHdl ht cont = 
       case ht of
        ClosedHandle  -> showsPrec p ht . showString "}"
	_ -> cont
       
    showBufMode :: Buffer -> BufferMode -> ShowS
    showBufMode buf bmo =
      case bmo of
        NoBuffering   -> showString "none"
	LineBuffering -> showString "line"
	BlockBuffering (Just n) -> showString "block " . showParen True (showsPrec p n)
	BlockBuffering Nothing  -> showString "block " . showParen True (showsPrec p def)
      where
       def :: Int 
       def = bufSize buf

-- ------------------------------------------------------------------------
-- Exception datatype and operations

data Exception
  = IOException 	IOException	-- IO exceptions
  | ArithException  	ArithException	-- Arithmetic exceptions
  | ArrayException	ArrayException  -- Array-related exceptions
  | ErrorCall		String		-- Calls to 'error'
  | ExitException	ExitCode	-- Call to System.exitWith
  | NoMethodError       String		-- A non-existent method was invoked
  | PatternMatchFail	String		-- A pattern match / guard failure
  | RecSelError		String		-- Selecting a non-existent field
  | RecConError		String		-- Field missing in record construction
  | RecUpdError		String		-- Record doesn't contain updated field
  | AssertionFailed	String		-- Assertions
  | DynException	Dynamic		-- Dynamic exceptions
  | AsyncException	AsyncException	-- Externally generated errors
  | BlockedOnDeadMVar			-- Blocking on a dead MVar
  | NonTermination
  | UserError		String

data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord)

data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  deriving (Eq, Ord)

data ArrayException
  = IndexOutOfBounds  	String		-- out-of-range array access
  | UndefinedElement	String		-- evaluating an undefined element
  deriving (Eq, Ord)

stackOverflow, heapOverflow :: Exception -- for the RTS
stackOverflow = AsyncException StackOverflow
heapOverflow  = AsyncException HeapOverflow

instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"

instance Show AsyncException where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
  showsPrec _ ThreadKilled    = showString "thread killed"

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s)
	= showString "array index out of range"
	. (if not (null s) then showString ": " . showString s
			   else id)
  showsPrec _ (UndefinedElement s)
	= showString "undefined array element"
	. (if not (null s) then showString ": " . showString s
			   else id)

instance Show Exception where
  showsPrec _ (IOException err)	         = shows err
  showsPrec _ (ArithException err)       = shows err
  showsPrec _ (ArrayException err)       = shows err
  showsPrec _ (ErrorCall err)	         = showString err
  showsPrec _ (ExitException err)        = showString "exit: " . shows err
  showsPrec _ (NoMethodError err)        = showString err
  showsPrec _ (PatternMatchFail err)     = showString err
  showsPrec _ (RecSelError err)	         = showString err
  showsPrec _ (RecConError err)	         = showString err
  showsPrec _ (RecUpdError err)	         = showString err
  showsPrec _ (AssertionFailed err)      = showString err
  showsPrec _ (DynException _err)        = showString "unknown exception"
  showsPrec _ (AsyncException e)	 = shows e
  showsPrec _ (BlockedOnDeadMVar)	 = showString "thread blocked indefinitely"
  showsPrec _ (NonTermination)           = showString "<<loop>>"
  showsPrec _ (UserError err)            = showString err

-- -----------------------------------------------------------------------------
-- The ExitCode type

-- The `ExitCode' type defines the exit codes that a program
-- can return.  `ExitSuccess' indicates successful termination;
-- and `ExitFailure code' indicates program failure
-- with value `code'.  The exact interpretation of `code'
-- is operating-system dependent.  In particular, some values of 
-- `code' may be prohibited (e.g. 0 on a POSIX-compliant system).

-- We need it here because it is used in ExitException in the
-- Exception datatype (above).

data ExitCode = ExitSuccess | ExitFailure Int 
                deriving (Eq, Ord, Read, Show)

-- --------------------------------------------------------------------------
-- Primitive throw

throw :: Exception -> a
throw exception = raise# exception

ioError         :: Exception -> IO a 
ioError err	=  IO $ \s -> throw err s

ioException	:: IOException -> IO a
ioException err =  IO $ \s -> throw (IOException err) s

-- ---------------------------------------------------------------------------
-- IOError type

-- A value @IOError@ encode errors occurred in the @IO@ monad.
-- An @IOError@ records a more specific error type, a descriptive
-- string and maybe the handle that was used when the error was
-- flagged.

type IOError = Exception

data IOException
 = IOError
     (Maybe Handle)   -- the handle used by the action flagging the
		      --   the error.
     IOErrorType      -- what it was.
     String	      -- location.
     String           -- error type specific information.
     (Maybe FilePath) -- filename the error is related to.

instance Eq IOException where
  (IOError h1 e1 loc1 str1 fn1) == (IOError h2 e2 loc2 str2 fn2) = 
    e1==e2 && str1==str2 && h1==h2 && loc1==loc2 && fn1==fn2

data IOErrorType
  -- Haskell 98:
  = AlreadyExists
  | EOF
  | IllegalOperation
  | NoSuchThing
  | PermissionDenied
  | ResourceBusy
  | ResourceExhausted
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
#if defined(cygwin32_TARGET_OS) || defined(mingw32_TARGET_OS)
  | ComError Int           -- HRESULT
#endif
  deriving (Eq)

instance Show IOErrorType where
  showsPrec _ e =
    showString $
    case e of
      AlreadyExists	-> "already exists"
      HardwareFault	-> "hardware fault"
      IllegalOperation	-> "illegal operation"
      InappropriateType -> "inappropriate type"
      Interrupted       -> "interrupted"
      InvalidArgument   -> "invalid argument"
      NoSuchThing       -> "does not exist"
      OtherError        -> "failed"
      PermissionDenied  -> "permission denied"
      ProtocolError     -> "protocol error"
      ResourceBusy      -> "resource busy"
      ResourceExhausted -> "resource exhausted"
      ResourceVanished  -> "resource vanished"
      SystemError	-> "system error"
      TimeExpired       -> "timeout"
      UnsatisfiedConstraints -> "unsatisified constraints" -- ultra-precise!
      UnsupportedOperation -> "unsupported operation"
      EOF		-> "end of file"
#if defined(cygwin32_TARGET_OS) || defined(mingw32_TARGET_OS)
      ComError _	-> "COM error"
#endif

userError       :: String  -> IOError
userError str	=  UserError str

-- ---------------------------------------------------------------------------
-- Predicates on IOError

isAlreadyExistsError :: IOError -> Bool
isAlreadyExistsError (IOException (IOError _ AlreadyExists _ _ _)) = True
isAlreadyExistsError _                                             = False

isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError (IOException (IOError _ ResourceBusy _ _ _)) = True
isAlreadyInUseError _                                            = False

isFullError :: IOError -> Bool
isFullError (IOException (IOError _ ResourceExhausted _ _ _)) = True
isFullError _                                                 = False

isEOFError :: IOError -> Bool
isEOFError (IOException (IOError _ EOF _ _ _)) = True
isEOFError _                                   = False

isIllegalOperation :: IOError -> Bool
isIllegalOperation (IOException (IOError _ IllegalOperation _ _ _)) = True
isIllegalOperation _                                                = False

isPermissionError :: IOError -> Bool
isPermissionError (IOException (IOError _ PermissionDenied _ _ _)) = True
isPermissionError _                                                = False

isDoesNotExistError :: IOError -> Bool
isDoesNotExistError (IOException (IOError _ NoSuchThing _ _ _)) = True
isDoesNotExistError _                                           = False

isUserError :: IOError -> Bool
isUserError (UserError _) = True
isUserError _             = False

-- ---------------------------------------------------------------------------
-- Showing IOErrors

instance Show IOException where
    showsPrec p (IOError hdl iot loc s fn) =
      showsPrec p iot .
      (case loc of
         "" -> id
	 _  -> showString "\nAction: " . showString loc) .
      (case hdl of
        Nothing -> id
	Just h  -> showString "\nHandle: " . showsPrec p h) .
      (case s of
	 "" -> id
	 _  -> showString "\nReason: " . showString s) .
      (case fn of
	 Nothing -> id
	 Just name -> showString "\nFile: " . showString name)
\end{code}
