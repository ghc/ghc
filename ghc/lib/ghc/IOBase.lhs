%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IOBase]{Module @IOBase@}

Definitions for the @IO@ monad and its friends.  Everything is exported
concretely; the @IO@ module itself exports abstractly.

\begin{code}
#include "error.h"

module IOBase where

import Prelude	()
import STBase
import PrelTup
import Foreign
import PackedString	( unpackCString )
import PrelBase
import GHC

infixr 1 `thenIO_Prim`
\end{code}

%*********************************************************
%*							*
\subsection{The @IO@ monad}
%*							*
%*********************************************************

\begin{code}
newtype IO a = IO (PrimIO (Either IOError a))

instance  Functor IO where
   map f x = x >>= (return . f)

instance  Monad IO  where
{-	No inlining for now... until we can inline some of the
	imports, like $, these functions are pretty big. 
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
-}
    m >> k      =  m >>= \ _ -> k
    return x	= IO $ ST $ \ s@(S# _) -> (Right x, s)

    (IO (ST m)) >>= k
      = IO $ ST $ \ s ->
	let  (r, new_s) = m s  in
	case r of
	  Left err -> (Left err, new_s)
	  Right  x -> case (k x) of { IO (ST k2) ->
		      k2 new_s }

fixIO :: (a -> IO a) -> IO a
    -- not required but worth having around

fixIO k = IO $ ST $ \ s ->
    let
	(IO (ST k_loop)) = k loop
	result           = k_loop s
	(Right loop, _)  = result
    in
    result

fail            :: IOError -> IO a 
fail err	=  IO $ ST $ \ s -> (Left err, s)

userError       :: String  -> IOError
userError str	=  UserError str

catch           :: IO a    -> (IOError -> IO a) -> IO a 
catch (IO (ST m)) k  = IO $ ST $ \ s ->
  case (m s) of { (r, new_s) ->
  case r of
    Right  _ -> (r, new_s)
    Left err -> case (k err) of { IO (ST k_err) ->
		(k_err new_s) }}

instance  Show (IO a)  where
    showsPrec p f  = showString "<<IO action>>"
    showList	   = showList__ (showsPrec 0)
\end{code}

%*********************************************************
%*							*
\subsection{Coercions to @ST@ and @PrimIO@}
%*							*
%*********************************************************

\begin{code}
stToIO	   :: ST RealWorld a -> IO a
primIOToIO :: PrimIO a       -> IO a
ioToST	   :: IO a -> ST RealWorld a
ioToPrimIO :: IO a -> PrimIO       a

primIOToIO = stToIO -- for backwards compatibility
ioToPrimIO = ioToST

stToIO (ST m) = IO $ ST $ \ s ->
    case (m s) of { (r, new_s) ->
    (Right r, new_s) }

ioToST (IO (ST io)) = ST $ \ s ->
    case (io s) of { (r, new_s) ->
    case r of
      Right a -> (a, new_s)
      Left  e -> error ("I/O Error (ioToST): " ++ showsPrec 0 e "\n")
    }
\end{code}

@thenIO_Prim@ is a useful little number for doing _ccall_s in IO-land:

\begin{code}
thenIO_Prim :: PrimIO a -> (a -> IO b) -> IO b
{-# INLINE thenIO_Prim   #-}

thenIO_Prim (ST m) k = IO $ ST $ \ s ->
    case (m s)     of { (m_res, new_s)    ->
    case (k m_res) of { (IO (ST k_m_res)) ->
    k_m_res new_s }}
\end{code}


%*********************************************************
%*							*
\subsection{Error/trace-ish functions}
%*							*
%*********************************************************

\begin{code}
errorIO :: PrimIO () -> a

errorIO (ST io)
  = case (errorIO# io) of
      _ -> bottom
  where
    bottom = bottom -- Never evaluated

-- error stops execution and displays an error message
error :: String -> a
error s = error__ ( \ x -> _ccall_ ErrorHdrHook x ) s

error__ :: (Addr{-FILE *-} -> PrimIO ()) -> String -> a

error__ msg_hdr s
#ifdef __PARALLEL_HASKELL__
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ stg_exit (1::Int)
	    )
#else
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ getErrorHandler	>>= \ errorHandler ->
	     if errorHandler == (-1::Int) then
		_ccall_ stg_exit (1::Int)
	     else
		_casm_ ``%r = (StgStablePtr)(%0);'' errorHandler
						>>= \ osptr ->
		_ccall_ decrementErrorCount     >>= \ () ->
		deRefStablePtr osptr            >>= \ oact ->
		oact
	    )
#endif {- !parallel -}
  where
    sTDERR = (``stderr'' :: Addr)
\end{code}

\begin{code}
{-# GENERATE_SPECS _trace a #-}
trace :: String -> a -> a

trace string expr
  = unsafePerformPrimIO (
	((_ccall_ PreTraceHook sTDERR{-msg-}):: PrimIO ())  >>
	fputs sTDERR string				    >>
	((_ccall_ PostTraceHook sTDERR{-msg-}):: PrimIO ()) >>
	returnPrimIO expr )
  where
    sTDERR = (``stderr'' :: Addr)
\end{code}


%*********************************************************
%*							*
\subsection{Utility functions}
%*							*
%*********************************************************

The construct $try comp$ exposes errors which occur within a
computation, and which are not fully handled.  It always succeeds.
This one didn't make it into the 1.3 defn

\begin{code}
tryIO :: IO a -> IO (Either IOError a) 
tryIO p = catch (p >>= (return . Right)) (return . Left)
\end{code}

I'm not sure why this little function is here...

\begin{code}
fputs :: Addr{-FILE*-} -> String -> PrimIO Bool

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

\begin{code}
data IOError
  = AlreadyExists		String
  | HardwareFault		String
  | IllegalOperation		String
  | InappropriateType		String
  | Interrupted			String
  | InvalidArgument		String
  | NoSuchThing			String
  | OtherError			String
  | PermissionDenied		String
  | ProtocolError		String
  | ResourceBusy		String
  | ResourceExhausted		String
  | ResourceVanished		String
  | SystemError			String
  | TimeExpired			String
  | UnsatisfiedConstraints	String
  | UnsupportedOperation	String
  | UserError			String
  | EOF

instance Eq IOError where
    -- I don't know what the (pointless) idea is here,
    -- presumably just compare them by their tags (WDP)
    a == b = tag a == tag b
      where
	tag (AlreadyExists _)		= (1::Int)
	tag (HardwareFault _)		= 2
	tag (IllegalOperation _)	= 3
	tag (InappropriateType _)	= 4
	tag (Interrupted _)		= 5
	tag (InvalidArgument _)		= 6
	tag (NoSuchThing _)		= 7
	tag (OtherError _)		= 8
	tag (PermissionDenied _)	= 9
	tag (ProtocolError _)		= 10
	tag (ResourceBusy _)		= 11
	tag (ResourceExhausted _)	= 12
	tag (ResourceVanished _)	= 13
	tag (SystemError _)		= 14
	tag (TimeExpired _)		= 15
	tag (UnsatisfiedConstraints _)	= 16
	tag (UnsupportedOperation _)	= 17
	tag (UserError _)		= 18
	tag EOF				= 19
\end{code}

Predicates on IOError; almost no effort made on these so far...

\begin{code}

isAlreadyExistsError (AlreadyExists _) = True
isAlreadyExistsError _		       = False

isAlreadyInUseError (ResourceBusy _) = True
isAlreadyInUseError _		     = False

isFullError (ResourceExhausted _) = True
isFullError _			  = False

isEOFError EOF = True
isEOFError _   = True

isIllegalOperation (IllegalOperation _) = True
isIllegalOperation _			= False

isPermissionError (PermissionDenied _)	= True
isPermissionError _			= False

isUserError (UserError s) = Just s
isUserError _		  = Nothing
\end{code}

Showing @IOError@s

\begin{code}
instance Show IOError where
    showsPrec p (AlreadyExists s)	= show2 "AlreadyExists: "	s
    showsPrec p (HardwareFault s)	= show2 "HardwareFault: "	s
    showsPrec p (IllegalOperation s)	= show2 "IllegalOperation: "	s
    showsPrec p (InappropriateType s)	= show2 "InappropriateType: "	s
    showsPrec p (Interrupted s)		= show2 "Interrupted: "		s
    showsPrec p (InvalidArgument s)	= show2 "InvalidArgument: "	s
    showsPrec p (NoSuchThing s)		= show2 "NoSuchThing: "		s
    showsPrec p (OtherError s)		= show2 "OtherError: "		s
    showsPrec p (PermissionDenied s)	= show2 "PermissionDenied: "	s
    showsPrec p (ProtocolError s)	= show2 "ProtocolError: "	s
    showsPrec p (ResourceBusy s)	= show2 "ResourceBusy: "	s
    showsPrec p (ResourceExhausted s)	= show2 "ResourceExhausted: "	s
    showsPrec p (ResourceVanished s)	= show2 "ResourceVanished: "	s
    showsPrec p (SystemError s)		= show2 "SystemError: "		s
    showsPrec p (TimeExpired s)		= show2 "TimeExpired: "		s
    showsPrec p (UnsatisfiedConstraints s) = show2 "UnsatisfiedConstraints: " s
    showsPrec p (UnsupportedOperation s)= show2 "UnsupportedOperation: " s
    showsPrec p (UserError s)		= showString s
    showsPrec p EOF			= showString "EOF"

show2 x y = showString x . showString y

{-

The @String@ part of an @IOError@ is platform-dependent.  However, to
provide a uniform mechanism for distinguishing among errors within
these broad categories, each platform-specific standard shall specify
the exact strings to be used for particular errors.  For errors not
explicitly mentioned in the standard, any descriptive string may be
used.

  SOF 4/96 - added argument to indicate function that flagged error
-}
constructErrorAndFail :: String -> IO a
constructError	      :: String -> PrimIO IOError

constructErrorAndFail call_site
  = stToIO (constructError call_site) >>= \ io_error ->
    fail io_error

constructError call_site
  = _casm_ ``%r = ghc_errtype;''    >>= \ (I# errtype#) ->
    _casm_ ``%r = ghc_errstr;''	    >>= \ str ->
    let
	msg = call_site ++ ':' : ' ' : unpackCString str
    in
    return (case errtype# of
	ERR_ALREADYEXISTS#		-> AlreadyExists msg
	ERR_HARDWAREFAULT#		-> HardwareFault msg
	ERR_ILLEGALOPERATION#		-> IllegalOperation msg
	ERR_INAPPROPRIATETYPE#		-> InappropriateType msg
	ERR_INTERRUPTED#		-> Interrupted msg
	ERR_INVALIDARGUMENT#		-> InvalidArgument msg
	ERR_NOSUCHTHING#		-> NoSuchThing msg
	ERR_OTHERERROR#			-> OtherError msg
	ERR_PERMISSIONDENIED#		-> PermissionDenied msg
	ERR_PROTOCOLERROR#		-> ProtocolError msg
	ERR_RESOURCEBUSY#		-> ResourceBusy msg
	ERR_RESOURCEEXHAUSTED#		-> ResourceExhausted msg
	ERR_RESOURCEVANISHED#		-> ResourceVanished msg
	ERR_SYSTEMERROR#		-> SystemError msg
	ERR_TIMEEXPIRED#		-> TimeExpired msg
	ERR_UNSATISFIEDCONSTRAINTS#	-> UnsatisfiedConstraints msg
	ERR_UNSUPPORTEDOPERATION#	-> UnsupportedOperation msg
	ERR_EOF#			-> EOF
	_				-> OtherError "bad error construct"
    )
\end{code}


