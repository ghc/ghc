%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelIOErr]{I/O Errors}

Haskell 1.2 does not provide standard error values for I/O errors.
This is unacceptable for portable implementations which indulge in
non-trivial I/O.  The $IOError$ type has therefore been extended from
Haskell 1.2, and possible error values have been identified for all
standard operations.

\begin{code}
#include "error.h"

module PreludeIOError (IOError13(..), _constructError) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( (++) )
import PS
import Prel		( (.), not )
import PreludeGlaST
import Text

data IOError13 = AlreadyExists String
             | HardwareFault String
             | IllegalOperation String
             | InappropriateType String
	     | Interrupted String
             | InvalidArgument String
             | NoSuchThing String
             | OtherError13 String
             | PermissionDenied String
             | ProtocolError String
             | ResourceBusy String
             | ResourceExhausted String
             | ResourceVanished String
             | SystemError String
             | TimeExpired String
             | UnsatisfiedConstraints String
             | UnsupportedOperation String
             | UserError String
	     | EOF
	     -- not really OK: deriving (Text)

instance Text IOError13 where -- NB: not interested in reading
    showsPrec p (AlreadyExists s)	= show2 "AlreadyExists: "	s
    showsPrec p (HardwareFault s)	= show2 "HardwareFault: "	s
    showsPrec p (IllegalOperation s)	= show2 "IllegalOperation: "	s
    showsPrec p (InappropriateType s)	= show2 "InappropriateType: "	s
    showsPrec p (Interrupted s)		= show2 "Interrupted: "		s
    showsPrec p (InvalidArgument s)	= show2 "InvalidArgument: "	s
    showsPrec p (NoSuchThing s)		= show2 "NoSuchThing: "		s
    showsPrec p (OtherError13 s)	= show2 "OtherError13: "	s
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

_constructError :: PrimIO IOError13
_constructError =
    _casm_ ``%r = ghc_errtype;''          `thenPrimIO` \ (I# errtype#) ->
    _casm_ ``%r = ghc_errstr;''           `thenPrimIO` \ str ->
    let
      msg = _unpackPS (_packCString str)
    in
        returnPrimIO (case errtype# of
          ERR_ALREADYEXISTS# -> AlreadyExists msg
          ERR_HARDWAREFAULT# -> HardwareFault msg
          ERR_ILLEGALOPERATION# -> IllegalOperation msg
          ERR_INAPPROPRIATETYPE# -> InappropriateType msg
	  ERR_INTERRUPTED# -> Interrupted msg
          ERR_INVALIDARGUMENT# -> InvalidArgument msg
          ERR_NOSUCHTHING# -> NoSuchThing msg
          ERR_OTHERERROR# -> OtherError13 msg
          ERR_PERMISSIONDENIED# -> PermissionDenied msg
          ERR_PROTOCOLERROR# -> ProtocolError msg
          ERR_RESOURCEBUSY# -> ResourceBusy msg
          ERR_RESOURCEEXHAUSTED# -> ResourceExhausted msg
          ERR_RESOURCEVANISHED# -> ResourceVanished msg
          ERR_SYSTEMERROR# -> SystemError msg
          ERR_TIMEEXPIRED# -> TimeExpired msg
          ERR_UNSATISFIEDCONSTRAINTS# -> UnsatisfiedConstraints msg
          ERR_UNSUPPORTEDOPERATION# -> UnsupportedOperation msg
          ERR_EOF# -> EOF
          _ -> OtherError13 "bad error construct"
        )
\end{code}

The $String$ part of an $IOError13$ is platform-dependent.  However, to
provide a uniform mechanism for distinguishing among errors within
these broad categories, each platform-specific standard shall specify
the exact strings to be used for particular errors.  For errors not
explicitly mentioned in the standard, any descriptive string may be
used.
