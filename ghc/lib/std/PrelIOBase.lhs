% -----------------------------------------------------------------------------
% $Id: PrelIOBase.lhs,v 1.21 2000/04/10 16:02:58 simonpj Exp $
% 
% (c) The AQUA Project, Glasgow University, 1994-1998
%

\section[PrelIOBase]{Module @PrelIOBase@}

Definitions for the @IO@ monad and its friends.  Everything is exported
concretely; the @IO@ module itself exports abstractly.

\begin{code}
{-# OPTIONS -fcompiling-prelude -fno-implicit-prelude -#include "cbits/stgio.h" #-}
#include "cbits/stgerror.h"
#include "config.h"

#ifndef __HUGS__ /* Hugs just includes this in PreludeBuiltin so no header needed */
module PrelIOBase where

import {-# SOURCE #-} PrelErr ( error )

import PrelST
import PrelBase
import {-# SOURCE #-} PrelException ( ioError )
import PrelMaybe  ( Maybe(..) )
import PrelAddr	  ( Addr(..), nullAddr )
import PrelPack ( unpackCString )
import PrelShow

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
#define FILE_OBJECT	    ForeignObj
#else
#define FILE_OBJECT	    Addr
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
    fail s	= error s -- not ioError?

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
stToIO	      :: ST RealWorld a -> IO a
stToIO (ST m) = (IO m)

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

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = stToIO . unsafeInterleaveST . ioToST
#endif
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
data IOError 
 = IOError 
     (Maybe Handle)  -- the handle used by the action flagging the
		     -- the error.
     IOErrorType     -- what it was.
     String	     -- location
     String          -- error type specific information.

instance Eq IOError where
  (IOError h1 e1 loc1 str1) == (IOError h2 e2 loc2 str2) = 
    e1==e2 && str1==str2 && h1==h2 && loc1 == loc2

data IOErrorType
  = AlreadyExists        | HardwareFault
  | IllegalOperation     | InappropriateType
  | Interrupted          | InvalidArgument
  | NoSuchThing          | OtherError
  | PermissionDenied     | ProtocolError
  | ResourceBusy         | ResourceExhausted
  | ResourceVanished     | SystemError
  | TimeExpired          | UnsatisfiedConstraints
  | UnsupportedOperation | UserError
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
      UserError         -> "failed"
      UnsupportedOperation -> "unsupported operation"
      EOF		-> "end of file"
#if defined(cygwin32_TARGET_OS) || defined(mingw32_TARGET_OS)
      ComError _	-> "COM error"
#endif



userError       :: String  -> IOError
userError str	=  IOError Nothing UserError "" str
\end{code}

Predicates on IOError; little effort made on these so far...

\begin{code}

isAlreadyExistsError :: IOError -> Bool
isAlreadyExistsError (IOError _ AlreadyExists _ _) = True
isAlreadyExistsError _		                   = False

isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError (IOError _ ResourceBusy _ _) = True
isAlreadyInUseError _		                 = False

isFullError :: IOError -> Bool
isFullError (IOError _ ResourceExhausted _ _) = True
isFullError _			              = False

isEOFError :: IOError -> Bool
isEOFError (IOError _ EOF _ _) = True
isEOFError _                   = False

isIllegalOperation :: IOError -> Bool
isIllegalOperation (IOError _ IllegalOperation _ _) = True
isIllegalOperation _			            = False

isPermissionError :: IOError -> Bool
isPermissionError (IOError _ PermissionDenied _ _) = True
isPermissionError _			           = False

isDoesNotExistError :: IOError -> Bool
isDoesNotExistError (IOError _ NoSuchThing _ _) = True
isDoesNotExistError _                           = False

isUserError :: IOError -> Bool
isUserError (IOError _ UserError _ _) = True
isUserError _		              = False
\end{code}

Showing @IOError@s

\begin{code}
#ifdef __HUGS__
-- For now we give a fairly uninformative error message which just happens to
-- be like the ones that Hugs used to give.
instance Show IOError where
    showsPrec p (IOError hdl iot loc s) = showString s . showChar '\n'
#else
instance Show IOError where
    showsPrec p (IOError hdl iot loc s) =
      showsPrec p iot .
      showChar '\n' .
      (case loc of
         "" -> id
	 _  -> showString "Action: " . showString loc . showChar '\n') .
      showHdl .
      (case s of
	 "" -> id
	 _  -> showString "Reason: " . showString s)
     where
      showHdl = 
       case hdl of
        Nothing -> id
	Just h  -> showString "Handle: " . showsPrec p h

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
    ioError io_error

constructErrorAndFailWithInfo :: String -> String -> IO a
constructErrorAndFailWithInfo call_site reason
  = constructErrorMsg call_site (Just reason) >>= \ io_error ->
    ioError io_error

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
constructError	      :: String -> IO IOError
constructError call_site = constructErrorMsg call_site Nothing

constructErrorMsg	      :: String -> Maybe String -> IO IOError
constructErrorMsg call_site reason =
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
     ERR_UNSATISFIEDCONSTRAINTS -> UnsatisfiedConstraints
     ERR_UNSUPPORTEDOPERATION   -> UnsupportedOperation
     ERR_EOF		         -> EOF
     _			         -> OtherError

  msg = 
   unpackCString str ++
   (case iot of
     OtherError -> "(error code: " ++ show errtype ++ ")"
     _ -> "") ++
   (case reason of
      Nothing -> ""
      Just m  -> ' ':m)
 in
 return (IOError Nothing iot call_site msg)
\end{code}

File names are specified using @FilePath@, a OS-dependent
string that (hopefully, I guess) maps to an accessible file/object.

\begin{code}
type FilePath = String
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
  Double sigh - ForeignObj is needed here too to break a cycle.
-}
data ForeignObj = ForeignObj ForeignObj#   -- another one
instance CCallable ForeignObj

eqForeignObj :: ForeignObj  -> ForeignObj -> Bool
eqForeignObj mp1 mp2
  = unsafePerformIO (primEqForeignObj mp1 mp2) /= (0::Int)

foreign import "eqForeignObj" unsafe primEqForeignObj :: ForeignObj -> ForeignObj -> IO Int

instance Eq ForeignObj where 
    p == q = eqForeignObj p q
    p /= q = not (eqForeignObj p q)
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
      haFilePath__    :: FilePath
    }      

{-
  Internally, we classify handles as being one
  of the following:
-}
data Handle__Type
 = ErrorHandle  IOError
 | ClosedHandle
 | SemiClosedHandle
 | ReadHandle
 | WriteHandle
 | AppendHandle
 | ReadWriteHandle


-- handle types are 'show'ed when printing error msgs, so
-- we provide a more user-friendly Show instance for it
-- than the derived one.
instance Show Handle__Type where
  showsPrec p t =
    case t of
      ErrorHandle iot   -> showString "error " . showsPrec p iot
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
	     case h               of { MVar h# ->
	     case takeMVar# h# s# of { (# s2# , r #) -> 
		    (# s2#, r #) }})
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
        ErrorHandle _ -> showsPrec p ht . showString "}\n"
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

mkBuffer__ :: FILE_OBJECT -> Int -> IO ()
mkBuffer__ fo sz_in_bytes = do
 chunk <- 
  case sz_in_bytes of
    0 -> return nullAddr  -- this has the effect of overwriting the pointer to the old buffer.
    _ -> do
     chunk <- allocMemory__ sz_in_bytes
     if chunk == nullAddr
      then ioError (IOError Nothing ResourceExhausted "mkBuffer__" "not enough virtual memory")
      else return chunk
 setBuf fo chunk sz_in_bytes

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
foreign import "libHS_cbits" "getErrStr__"  unsafe getErrStr__  :: IO Addr 
foreign import "libHS_cbits" "getErrNo__"   unsafe getErrNo__   :: IO Int  
foreign import "libHS_cbits" "getErrType__" unsafe getErrType__ :: IO Int  

foreign import "libHS_cbits" "allocMemory__" unsafe
           allocMemory__    :: Int -> IO Addr
foreign import "libHS_cbits" "getBufSize"  unsafe
           getBufSize       :: FILE_OBJECT -> IO Int
foreign import "libHS_cbits" "setBuf" unsafe
           setBuf       :: FILE_OBJECT -> Addr -> Int -> IO ()

\end{code}
