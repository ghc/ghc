% ------------------------------------------------------------------------------
% $Id: PrelIOBase.lhs,v 1.36 2001/02/27 13:38:15 simonmar Exp $
% 
% (c) The University of Glasgow, 1994-2000
%

\section[PrelIOBase]{Module @PrelIOBase@}

Definitions for the @IO@ monad and its friends.  Everything is exported
concretely; the @IO@ module itself exports abstractly.

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}
#include "config.h"
#include "cbits/stgerror.h"

#ifndef __HUGS__ /* Hugs just includes this in PreludeBuiltin so no header needed */
module PrelIOBase where

import {-# SOURCE #-} PrelErr ( error )

import PrelST
import PrelBase
import PrelNum	-- To get fromInteger etc, needed because of -fno-implicit-prelude
import PrelMaybe  ( Maybe(..) )
import PrelShow
import PrelList
import PrelDynamic
import PrelPtr
import PrelPack ( unpackCString )

#if !defined(__CONCURRENT_HASKELL__)
import PrelArr	  ( MutableVar, readVar )
#endif
#endif

#ifdef __HUGS__
#define __CONCURRENT_HASKELL__
#define stToIO id
#define unpackCString primUnpackString
#endif

#ifndef __PARALLEL_HASKELL__
#define FILE_OBJECT	    (ForeignPtr ())
#else
#define FILE_OBJECT	    (Ptr ())

#endif
\end{code}

%*********************************************************
%*							*
\subsection{The @IO@ monad}
%*							*
%*********************************************************

The IO Monad is just an instance of the ST monad, where the state is
the real world.  We use the exception mechanism (in PrelException) to
implement IO exceptions.

NOTE: The IO representation is deeply wired in to various parts of the
system.  The following list may or may not be exhaustive:

Compiler  - types of various primitives in PrimOp.lhs

RTS 	  - forceIO (StgMiscClosures.hc)
	  - catchzh_fast, (un)?blockAsyncExceptionszh_fast, raisezh_fast 
	    (Exceptions.hc)
	  - raiseAsync (Schedule.c)

Prelude   - PrelIOBase.lhs, and several other places including
	    PrelException.lhs.

Libraries - parts of hslibs/lang.

--SDM

\begin{code}
#ifndef __HUGS__
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
    fail s	= ioError (userError s)

liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO ( \ s ->
  case m s of 
    (# new_s, a #) -> unIO (k a) new_s
  )

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Coercions to @ST@}
%*							*
%*********************************************************

\begin{code}
#ifdef __HUGS__
/* Hugs doesn't distinguish these types so no coercion required) */
#else
-- stToIO     :: (forall s. ST s a) -> IO a
stToIO	      :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

ioToST	      :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Unsafe @IO@ operations}
%*							*
%*********************************************************

\begin{code}
#ifndef __HUGS__
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
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Types @Handle@, @Handle__@}
%*							*
%*********************************************************

The type for @Handle@ is defined rather than in @IOHandle@
module, as the @IOError@ type uses it..all operations over
a handles reside in @IOHandle@.

\begin{code}

#ifndef __HUGS__
{-
 Sigh, the MVar ops in ConcBase depend on IO, the IO
 representation here depend on MVars for handles (when
 compiling in a concurrent way). Break the cycle by having
 the definition of MVars go here:

-}
data MVar a = MVar (MVar# RealWorld a)

-- pull in Eq (Mvar a) too, to avoid PrelConc being an orphan-instance module
instance Eq (MVar a) where
	(MVar mvar1#) == (MVar mvar2#) = sameMVar# mvar1# mvar2#

{-
  Double sigh - ForeignPtr is needed here too to break a cycle.
-}
data ForeignPtr a = ForeignPtr ForeignObj#
instance CCallable (ForeignPtr a)

eqForeignPtr :: ForeignPtr a -> ForeignPtr a -> Bool
eqForeignPtr mp1 mp2
  = unsafePerformIO (primEqForeignPtr mp1 mp2) /= (0::Int)

foreign import "eqForeignObj" unsafe 
  primEqForeignPtr :: ForeignPtr a -> ForeignPtr a -> IO Int

instance Eq (ForeignPtr a) where 
    p == q = eqForeignPtr p q
    p /= q = not (eqForeignPtr p q)
#endif /* ndef __HUGS__ */

#if defined(__CONCURRENT_HASKELL__)
newtype Handle = Handle (MVar Handle__)
#else
newtype Handle = Handle (MutableVar RealWorld Handle__)
#endif

instance Eq Handle where
 (Handle h1) == (Handle h2) = h1 == h2

{-
  A Handle is represented by (a reference to) a record 
  containing the state of the I/O port/device. We record
  the following pieces of info:

    * type (read,write,closed etc.)
    * pointer to the external file object.
    * buffering mode 
    * user-friendly name (usually the
      FilePath used when IO.openFile was called)

Note: when a Handle is garbage collected, we want to flush its buffer
and close the OS file handle, so as to free up a (precious) resource.
-}
data Handle__
  = Handle__ {
      haFO__	      :: FILE_OBJECT,
      haType__        :: Handle__Type,
      haBufferMode__  :: BufferMode,
      haFilePath__    :: FilePath,
      haBuffers__     :: [Ptr ()]
    }

{-
  Internally, we classify handles as being one
  of the following:
-}
data Handle__Type
 = ClosedHandle
 | SemiClosedHandle
 | ReadHandle
 | WriteHandle
 | AppendHandle
 | ReadWriteHandle


-- File names are specified using @FilePath@, a OS-dependent
-- string that (hopefully, I guess) maps to an accessible file/object.

type FilePath = String
\end{code}

%*********************************************************
%*							*
\subsection[Show-Handle]{Show instance for Handles}
%*							*
%*********************************************************

\begin{code}
-- handle types are 'show'ed when printing error msgs, so
-- we provide a more user-friendly Show instance for it
-- than the derived one.
instance Show Handle__Type where
  showsPrec p t =
    case t of
      ClosedHandle      -> showString "closed"
      SemiClosedHandle  -> showString "semi-closed"
      ReadHandle        -> showString "readable"
      WriteHandle       -> showString "writeable"
      AppendHandle      -> showString "writeable (append)"
      ReadWriteHandle   -> showString "read-writeable"

instance Show Handle where 
  showsPrec p (Handle h) = 
    let
#if defined(__CONCURRENT_HASKELL__)
#ifdef __HUGS__
     hdl_ = unsafePerformIO (primTakeMVar h)
#else
     -- (Big) SIGH: unfolded defn of takeMVar to avoid
     -- an (oh-so) unfortunate module loop with PrelConc.
     hdl_ = unsafePerformIO (IO $ \ s# ->
	     case h                 of { MVar h# ->
	     case takeMVar# h# s#   of { (# s2# , r #) -> 
	     case putMVar# h# r s2# of { s3# ->
	     (# s3#, r #) }}})
#endif
#else
     hdl_ = unsafePerformIO (stToIO (readVar h))
#endif
    in
    showChar '{' . 
    showHdl (haType__ hdl_) 
	    (showString "loc=" . showString (haFilePath__ hdl_) . showChar ',' .
	     showString "type=" . showsPrec p (haType__ hdl_) . showChar ',' .
	     showString "buffering=" . showBufMode (haFO__ hdl_) (haBufferMode__ hdl_) . showString "}\n" )
   where
    showHdl :: Handle__Type -> ShowS -> ShowS
    showHdl ht cont = 
       case ht of
        ClosedHandle  -> showsPrec p ht . showString "}\n"
	_ -> cont
       
    showBufMode :: FILE_OBJECT -> BufferMode -> ShowS
    showBufMode fo bmo =
      case bmo of
        NoBuffering   -> showString "none"
	LineBuffering -> showString "line"
	BlockBuffering (Just n) -> showString "block " . showParen True (showsPrec p n)
	BlockBuffering Nothing  -> showString "block " . showParen True (showsPrec p def)
      where
       def :: Int 
       def = unsafePerformIO (getBufSize fo)
\end{code}

%*********************************************************
%*							*
\subsection[BufferMode]{Buffering modes}
%*							*
%*********************************************************

Three kinds of buffering are supported: line-buffering, 
block-buffering or no-buffering.  These modes have the following
effects. For output, items are written out from the internal
buffer according to the buffer mode:

\begin{itemize}
\item[line-buffering]  the entire output buffer is written
out whenever a newline is output, the output buffer overflows, 
a flush is issued, or the handle is closed.

\item[block-buffering] the entire output buffer is written out whenever 
it overflows, a flush is issued, or the handle
is closed.

\item[no-buffering] output is written immediately, and never stored
in the output buffer.
\end{itemize}

The output buffer is emptied as soon as it has been written out.

Similarly, input occurs according to the buffer mode for handle {\em hdl}.
\begin{itemize}
\item[line-buffering] when the input buffer for {\em hdl} is not empty,
the next item is obtained from the buffer;
otherwise, when the input buffer is empty,
characters up to and including the next newline
character are read into the buffer.  No characters
are available until the newline character is
available.
\item[block-buffering] when the input buffer for {\em hdl} becomes empty,
the next block of data is read into this buffer.
\item[no-buffering] the next input item is read and returned.
\end{itemize}

For most implementations, physical files will normally be block-buffered 
and terminals will normally be line-buffered. (the IO interface provides
operations for changing the default buffering of a handle tho.)

\begin{code}
data BufferMode  
 = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
   deriving (Eq, Ord, Show)
   {- Read instance defined in IO. -}

\end{code}

Foreign import declarations to helper routines:

\begin{code}
foreign import "libHS_cbits" "getErrStr__"  unsafe getErrStr__  :: IO (Ptr ())
foreign import "libHS_cbits" "getErrNo__"   unsafe getErrNo__   :: IO Int  
foreign import "libHS_cbits" "getErrType__" unsafe getErrType__ :: IO Int  
  
-- ToDo: use mallocBytes from PrelMarshal?
malloc :: Int -> IO (Ptr ())
malloc sz = do
  a <- _malloc sz
  if (a == nullPtr)
	then ioException (IOError Nothing ResourceExhausted
	    "malloc" "out of memory" Nothing)
	else return a

foreign import "malloc" unsafe _malloc :: Int -> IO (Ptr ())

foreign import "libHS_cbits" "getBufSize"  unsafe
           getBufSize       :: FILE_OBJECT -> IO Int
foreign import "libHS_cbits" "setBuf" unsafe
           setBuf       :: FILE_OBJECT -> Ptr () -> Int -> IO ()

\end{code}

%*********************************************************
%*							*
\subsection{Exception datatype and operations}
%*							*
%*********************************************************

\begin{code}
data Exception
  = IOException 	IOException	-- IO exceptions
  | ArithException  	ArithException	-- Arithmetic exceptions
  | ArrayException	ArrayException  -- Array-related exceptions
  | ErrorCall		String		-- Calls to 'error'
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
\end{code}

%*********************************************************
%*							*
\subsection{Primitive throw}
%*							*
%*********************************************************

\begin{code}
throw :: Exception -> a
throw exception = raise# exception

ioError         :: Exception -> IO a 
ioError err	=  IO $ \s -> throw err s

ioException	:: IOException -> IO a
ioException err =  IO $ \s -> throw (IOException err) s
\end{code}

%*********************************************************
%*							*
\subsection{Type @IOError@}
%*							*
%*********************************************************

A value @IOError@ encode errors occurred in the @IO@ monad.
An @IOError@ records a more specific error type, a descriptive
string and maybe the handle that was used when the error was
flagged.

\begin{code}
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
  = AlreadyExists        | HardwareFault
  | IllegalOperation     | InappropriateType
  | Interrupted          | InvalidArgument
  | NoSuchThing          | OtherError
  | PermissionDenied     | ProtocolError
  | ResourceBusy         | ResourceExhausted
  | ResourceVanished     | SystemError
  | TimeExpired          | UnsatisfiedConstraints
  | UnsupportedOperation
  | EOF
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
\end{code}

Predicates on IOError; little effort made on these so far...

\begin{code}

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
\end{code}

Showing @IOError@s

\begin{code}
#ifdef __HUGS__
-- For now we give a fairly uninformative error message which just happens to
-- be like the ones that Hugs used to give.
instance Show IOException where
    showsPrec p (IOError _ _ _ s _) = showString s . showChar '\n'
#else
instance Show IOException where
    showsPrec p (IOError hdl iot loc s fn) =
      showsPrec p iot .
      (case loc of
         "" -> id
	 _  -> showString "\nAction: " . showString loc) .
      showHdl .
      (case s of
	 "" -> id
	 _  -> showString "\nReason: " . showString s) .
      (case fn of
	 Nothing -> id
	 Just name -> showString "\nFile: " . showString name)
     where
      showHdl = 
       case hdl of
        Nothing -> id
	Just h  -> showString "\nHandle: " . showsPrec p h

#endif
\end{code}

The @String@ part of an @IOError@ is platform-dependent.  However, to
provide a uniform mechanism for distinguishing among errors within
these broad categories, each platform-specific standard shall specify
the exact strings to be used for particular errors.  For errors not
explicitly mentioned in the standard, any descriptive string may be
used.

\begin{code}
constructErrorAndFail :: String -> IO a
constructErrorAndFail call_site
  = constructError call_site >>= \ io_error ->
    ioError (IOException io_error)

constructErrorAndFailWithInfo :: String -> String -> IO a
constructErrorAndFailWithInfo call_site fn
  = constructErrorMsg call_site (Just fn) >>= \ io_error ->
    ioError (IOException io_error)

\end{code}

This doesn't seem to be documented/spelled out anywhere,
so here goes: (SOF)

The implementation of the IO prelude uses various C stubs
to do the actual interaction with the OS. The bandwidth
\tr{C<->Haskell} is somewhat limited, so the general strategy
for flaggging any errors (apart from possibly using the
return code of the external call), is to set the @ghc_errtype@
to a value that is one of the \tr{#define}s in @includes/error.h@.
@ghc_errstr@ holds a character string providing error-specific
information. Error constructing functions will then reach out
and grab these values when generating

\begin{code}
constructError	      :: String -> IO IOException
constructError call_site = constructErrorMsg call_site Nothing

constructErrorMsg	      :: String -> Maybe String -> IO IOException
constructErrorMsg call_site fn =
 getErrType__            >>= \ errtype ->
 getErrStr__             >>= \ str ->
 let
  iot =
   case (errtype::Int) of
     ERR_ALREADYEXISTS	         -> AlreadyExists
     ERR_HARDWAREFAULT	         -> HardwareFault
     ERR_ILLEGALOPERATION	 -> IllegalOperation
     ERR_INAPPROPRIATETYPE	 -> InappropriateType
     ERR_INTERRUPTED	         -> Interrupted
     ERR_INVALIDARGUMENT	 -> InvalidArgument
     ERR_NOSUCHTHING	         -> NoSuchThing
     ERR_OTHERERROR		 -> OtherError
     ERR_PERMISSIONDENIED	 -> PermissionDenied
     ERR_PROTOCOLERROR	         -> ProtocolError
     ERR_RESOURCEBUSY	         -> ResourceBusy
     ERR_RESOURCEEXHAUSTED	 -> ResourceExhausted
     ERR_RESOURCEVANISHED	 -> ResourceVanished
     ERR_SYSTEMERROR	         -> SystemError
     ERR_TIMEEXPIRED	         -> TimeExpired
     ERR_UNSATISFIEDCONSTRAINTS  -> UnsatisfiedConstraints
     ERR_UNSUPPORTEDOPERATION    -> UnsupportedOperation
     ERR_EOF		         -> EOF
     _			         -> OtherError

  msg = 
   unpackCString str ++
   (case iot of
     OtherError -> "(error code: " ++ show errtype ++ ")"
     _ -> "")
 in
 return (IOError Nothing iot call_site msg fn)
\end{code}
