%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelIOBase]{Module @PrelIOBase@}

Definitions for the @IO@ monad and its friends.  Everything is exported
concretely; the @IO@ module itself exports abstractly.

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}
#include "error.h"

module PrelIOBase where

import {-# SOURCE #-} PrelErr ( error )
import PrelBase
import PrelST	  ( ST(..), STret(..), StateAndPtr#(..) )
import PrelMaybe  ( Maybe(..) )
import PrelAddr	  ( Addr(..), nullAddr )
import PrelPack   ( unpackCString )
import PrelArr	  ( MutableVar, readVar )

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
userError str	=  IOError Nothing (UserError Nothing) "" str

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
stToIO (ST m) = IO $ \ s -> case (m s) of STret new_s r -> IOok new_s r

ioToST	   :: IO a -> ST RealWorld a
ioToST (IO io) = ST $ \ s ->
    case (io s) of
      IOok   new_s a -> STret new_s a
      IOfail new_s e -> error ("I/O Error (ioToST): " ++ showsPrec 0 e "\n")
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


data IOErrorType
  = AlreadyExists        | HardwareFault
  | IllegalOperation     | InappropriateType
  | Interrupted          | InvalidArgument
  | NoSuchThing          | OtherError
  | PermissionDenied     | ProtocolError
  | ResourceBusy         | ResourceExhausted
  | ResourceVanished     | SystemError
  | TimeExpired          | UnsatisfiedConstraints
  | UnsupportedOperation | UserError (Maybe Addr)
  | EOF
  deriving (Eq)

instance Show IOErrorType where
  showsPrec d e =
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
      UserError _       -> "failed"
      EOF		-> "end of file"

\end{code}

Predicates on IOError; little effort made on these so far...

\begin{code}

isAlreadyExistsError (IOError _ AlreadyExists _ _) = True
isAlreadyExistsError _		                   = False

isAlreadyInUseError (IOError _ ResourceBusy _ _) = True
isAlreadyInUseError _		                 = False

isFullError (IOError _ ResourceExhausted _ _) = True
isFullError _			              = False

isEOFError (IOError _ EOF _ _) = True
isEOFError _                   = False

isIllegalOperation (IOError _ IllegalOperation _ _) = True
isIllegalOperation _			            = False

isPermissionError (IOError _ PermissionDenied _ _) = True
isPermissionError _			           = False

isDoesNotExistError (IOError _ NoSuchThing _ _) = True
isDoesNotExistError _                           = False

isUserError (IOError _ (UserError _) _ _) = True
isUserError _		                  = False
\end{code}

Showing @IOError@s

\begin{code}
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
    fail io_error

constructErrorAndFailWithInfo :: String -> String -> IO a
constructErrorAndFailWithInfo call_site reason
  = constructErrorMsg call_site (Just reason) >>= \ io_error ->
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
information. Error constructing functions will then reach out
and grab these values when generating

\begin{code}
constructError	      :: String -> IO IOError
constructError call_site = constructErrorMsg call_site Nothing

constructErrorMsg	      :: String -> Maybe String -> IO IOError
constructErrorMsg call_site reason =
 _ccall_ getErrType__            >>= \ (I# errtype#) ->
 _ccall_ getErrStr__             >>= \ str ->
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
   unpackCString str ++
   (case iot of
     OtherError -> "(error code: " ++ show (I# errtype#) ++ ")"
     _ -> "") ++
   (case reason of
      Nothing -> ""
      Just m  -> ' ':m)
 in
 return (IOError Nothing iot call_site msg)
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
 compiling in a concurrent way). Break the cycle by having
 the definition of MVars go here:

-}
data MVar a = MVar (SynchVar# RealWorld a)

{-
  Double sigh - ForeignObj is needed here too to break a cycle.
-}
data ForeignObj = ForeignObj ForeignObj#   -- another one
instance CCallable ForeignObj
instance CCallable ForeignObj#

makeForeignObj  :: Addr        -> Addr       -> IO ForeignObj
makeForeignObj (A# obj) (A# finaliser) = IO ( \ s# ->
    case makeForeignObj# obj finaliser s# of
      StateAndForeignObj# s1# fo# -> IOok s1# (ForeignObj fo#))

data StateAndForeignObj# s  = StateAndForeignObj# (State# s) ForeignObj#


#if defined(__CONCURRENT_HASKELL__)
newtype Handle = Handle (MVar Handle__)
#else
newtype Handle = Handle (MutableVar RealWorld Handle__)
#endif

#ifndef __PARALLEL_HASKELL__
#define FILE_OBJECT	    ForeignObj
#else
#define FILE_OBJECT	    Addr
#endif

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

This means that the finaliser for the handle needs to have access to
the buffer and the OS file handle. The current implementation of foreign
objects requires that the finaliser is implemented in C, so to
arrange for this to happen, openFile() returns a pointer to a structure
big enough to hold the OS file handle and a pointer to the buffer.
This pointer is then wrapped up inside a ForeignObj, and finalised
as desired.

-}
data Handle__
  = Handle__ {
      haFO__	      :: FILE_OBJECT,
      haType__        :: Handle__Type,
      haBufferMode__  :: BufferMode,
      haFilePath__    :: String
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
     -- (Big) SIGH: unfolded defn of takeMVar to avoid
     -- an (oh-so) unfortunate module loop with PrelConc.
     hdl_ = unsafePerformIO (IO $ \ s# ->
	     case h               of { MVar h# ->
	     case takeMVar# h# s# of { StateAndPtr# s2# r -> 
		    IOok s2# r }})
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
       def = unsafePerformIO (_ccall_ getBufSize fo)


{-
 nullFile__ is only used for closed handles, plugging it in as
 a null file object reference.
-}
nullFile__ :: FILE_OBJECT
nullFile__ = 
#ifndef __PARALLEL_HASKELL__
    unsafePerformIO (makeForeignObj nullAddr nullAddr{-i.e., don't finalise-})
#else
    nullAddr
#endif


mkClosedHandle__ :: Handle__
mkClosedHandle__ = 
  Handle__ 
	   nullFile__
	   ClosedHandle 
	   NoBuffering
	   "closed file"

mkErrorHandle__ :: IOError -> Handle__
mkErrorHandle__ ioe =
  Handle__
           nullFile__ 
	   (ErrorHandle ioe)
	   NoBuffering
	   "error handle"

mkBuffer__ :: FILE_OBJECT -> Int -> IO ()
mkBuffer__ fo sz_in_bytes = do
 chunk <- 
  case sz_in_bytes of
    0 -> return nullAddr  -- this has the effect of overwriting the pointer to the old buffer.
    _ -> do
     chunk <- _ccall_ allocMemory__ sz_in_bytes
     if chunk == nullAddr
      then fail (IOError Nothing ResourceExhausted "mkBuffer__" "not enough virtual memory")
      else return chunk
 _ccall_ setBuf fo chunk sz_in_bytes

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

%*********************************************************
%*							*
\subsection{Unsafe @IO@ operations}
%*							*
%*********************************************************

\begin{code}
{-# NOINLINE unsafePerformIO #-}
unsafePerformIO	:: IO a -> a
unsafePerformIO (IO m)
  = case m realWorld# of
      IOok _ r   -> r
      IOfail _ e -> error ("unsafePerformIO: I/O error: " ++ show e ++ "\n")

{-# NOINLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO (IO m) = IO ( \ s ->
	let
	    IOok _ r = m s
	in
	IOok s r)

\end{code}
