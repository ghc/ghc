%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IOBase]{Module @IOBase@}

Definitions for the @IO@ monad and its friends.  Everything is exported
concretely; the @IO@ module itself exports abstractly.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
#include "error.h"

module IOBase where

import {-# SOURCE #-} Error
import STBase
import PrelTup
import Addr
import PackBase	( unpackCString )
import PrelBase
import ArrBase	( ByteArray(..), MutableVar(..) )

import GHC

\end{code}

%*********************************************************
%*							*
\subsection{The @IO@ monad}
%*							*
%*********************************************************

IO is no longer built on top of PrimIO (which used to be a specialised
version of the ST monad), instead it is now has its own type.  This is
purely for efficiency purposes, since we get to remove several levels
of lifting in the type of the monad.

\begin{code}
newtype IO a = IO (State# RealWorld -> IOResult a)

{-# INLINE unIO #-}
unIO (IO a) = a

data IOResult a = IOok   (State# RealWorld) a
		| IOfail (State# RealWorld) IOError

instance  Functor IO where
   map f x = x >>= (return . f)

instance  Monad IO  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      =  m >>= \ _ -> k
    return x	= IO $ \ s -> IOok s x

    (IO m) >>= k =
        IO $ \s ->
	case m s of
	    IOfail new_s err -> IOfail new_s err
	    IOok   new_s a   -> unIO (k a) new_s

fixIO :: (a -> IO a) -> IO a
    -- not required but worth having around

fixIO k = IO $ \ s ->
    let
	(IO k_loop) = k loop
	result      = k_loop s
	IOok _ loop = result
    in
    result

fail            :: IOError -> IO a 
fail err	=  IO $ \ s -> IOfail s err

userError       :: String  -> IOError
userError str	=  IOError Nothing UserError str

catch           :: IO a    -> (IOError -> IO a) -> IO a 
catch (IO m) k  = IO $ \ s ->
  case m s of
    IOok   new_s a -> IOok new_s a
    IOfail new_s e -> unIO (k e) new_s

instance  Show (IO a)  where
    showsPrec p f  = showString "<<IO action>>"
    showList	   = showList__ (showsPrec 0)
\end{code}

%*********************************************************
%*							*
\subsection{Coercions to @ST@}
%*							*
%*********************************************************

\begin{code}
stToIO	   :: ST RealWorld a -> IO a
ioToST	   :: IO a -> ST RealWorld a

stToIO (ST m) = IO $ \ s -> case (m s) of STret new_s r -> IOok new_s r

ioToST (IO io) = ST $ \ s ->
    case (io s) of
      IOok   new_s a -> STret new_s a
      IOfail new_s e -> error ("I/O Error (ioToST): " ++ showsPrec 0 e "\n")
\end{code}

%*********************************************************
%*							*
\subsection{Utility functions}
%*							*
%*********************************************************

I'm not sure why this little function is here...

\begin{code}
fputs :: Addr{-FILE*-} -> String -> IO Bool

fputs stream [] = return True

fputs stream (c : cs)
  = _ccall_ stg_putc c stream >> -- stg_putc expands to putc
    fputs stream cs		 -- (just does some casting stream)
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
     String          -- error type specific information.


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
  deriving (Eq, Show)

\end{code}

Predicates on IOError; little effort made on these so far...

\begin{code}

isAlreadyExistsError (IOError _ AlreadyExists _) = True
isAlreadyExistsError _		                 = False

isAlreadyInUseError (IOError _ ResourceBusy _) = True
isAlreadyInUseError _		               = False

isFullError (IOError _ ResourceExhausted _) = True
isFullError _			            = False

isEOFError (IOError _ EOF _) = True
isEOFError _                 = True

isIllegalOperation (IOError _ IllegalOperation _) = True
isIllegalOperation _			          = False

isPermissionError (IOError _ PermissionDenied _) = True
isPermissionError _			         = False

isDoesNotExistError (IOError _ NoSuchThing _) = True
isDoesNotExistError _                         = False

isUserError (IOError _ UserError s) = Just s
isUserError _		            = Nothing
\end{code}

Showing @IOError@s

\begin{code}
instance Show IOError where
    showsPrec p (IOError _ UserError s) rs =
      showString s rs
{-
    showsPrec p (IOError _ EOF _) rs =
      showsPrec p EOF rs
-}
    showsPrec p (IOError _ iot s) rs =
      showsPrec p 
                iot 
                (case s of { 
		  "" -> rs; 
		  _ -> showString ": " $ 
		       showString s rs})

\end{code}

The @String@ part of an @IOError@ is platform-dependent.  However, to
provide a uniform mechanism for distinguishing among errors within
these broad categories, each platform-specific standard shall specify
the exact strings to be used for particular errors.  For errors not
explicitly mentioned in the standard, any descriptive string may be
used.

\begin{change}
SOF & 4/96 & added argument to indicate function that flagged error
\end{change}
% Hmm..does these envs work?!...SOF

\begin{code}
constructErrorAndFail :: String -> IO a
constructErrorAndFail call_site
  = constructError call_site >>= \ io_error ->
    fail io_error

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
information.

\begin{code}
constructError	      :: String -> IO IOError
constructError call_site =
 _casm_ ``%r = ghc_errtype;''    >>= \ (I# errtype#) ->
 _casm_ ``%r = ghc_errstr;''	 >>= \ str ->
 let
  iot =
   case errtype# of
     ERR_ALREADYEXISTS#	         -> AlreadyExists
     ERR_HARDWAREFAULT#	         -> HardwareFault
     ERR_ILLEGALOPERATION#	 -> IllegalOperation
     ERR_INAPPROPRIATETYPE#	 -> InappropriateType
     ERR_INTERRUPTED#	         -> Interrupted
     ERR_INVALIDARGUMENT#	 -> InvalidArgument
     ERR_NOSUCHTHING#	         -> NoSuchThing
     ERR_OTHERERROR#		 -> OtherError
     ERR_PERMISSIONDENIED#	 -> PermissionDenied
     ERR_PROTOCOLERROR#	         -> ProtocolError
     ERR_RESOURCEBUSY#	         -> ResourceBusy
     ERR_RESOURCEEXHAUSTED#	 -> ResourceExhausted
     ERR_RESOURCEVANISHED#	 -> ResourceVanished
     ERR_SYSTEMERROR#	         -> SystemError
     ERR_TIMEEXPIRED#	         -> TimeExpired
     ERR_UNSATISFIEDCONSTRAINTS# -> UnsatisfiedConstraints
     ERR_UNSUPPORTEDOPERATION#   -> UnsupportedOperation
     ERR_EOF#		         -> EOF
     _			         -> OtherError

  msg = 
   call_site ++ ':' : ' ' : unpackCString str ++
   case iot of
     OtherError -> "(error code: " ++ show (I# errtype#) ++ ")"
     _ -> ""
 in
 return (IOError Nothing iot msg)
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

{-
 Sigh, the MVar ops in ConcBase depend on IO, the IO
 representation here depend on MVars for handles (when
 compiling a concurrent way). Break the cycle by having
 the definition of MVars go here:

-}
data MVar a = MVar (SynchVar# RealWorld a)

{-
  Double sigh - ForeignObj is needed here too to break a cycle.
-}
data ForeignObj = ForeignObj ForeignObj#   -- another one

#if defined(__CONCURRENT_HASKELL__)
type Handle = MVar Handle__
#else
type Handle = MutableVar RealWorld Handle__
#endif

data Handle__
  = ErrorHandle		IOError
  | ClosedHandle
#ifndef __PARALLEL_HASKELL__
  | SemiClosedHandle	ForeignObj (Addr, Int)
  | ReadHandle		ForeignObj (Maybe BufferMode) Bool
  | WriteHandle		ForeignObj (Maybe BufferMode) Bool
  | AppendHandle	ForeignObj (Maybe BufferMode) Bool
  | ReadWriteHandle	ForeignObj (Maybe BufferMode) Bool
#else
  | SemiClosedHandle	Addr (Addr, Int)
  | ReadHandle		Addr (Maybe BufferMode) Bool
  | WriteHandle		Addr (Maybe BufferMode) Bool
  | AppendHandle	Addr (Maybe BufferMode) Bool
  | ReadWriteHandle	Addr (Maybe BufferMode) Bool
#endif

-- Standard Instances as defined by the Report..
-- instance Eq Handle   (defined in IO)
-- instance Show Handle    ""

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
and terminals will normally be line-buffered.

\begin{code}
data BufferMode  
 = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
   deriving (Eq, Ord, {-ToDo: Read,-} Show)
\end{code}

\begin{code}
performGC :: IO ()
performGC = _ccall_GC_ StgPerformGarbageCollection
\end{code}
