%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
\section[BasicTypes]{Miscellanous types}

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}

\begin{code}
module BasicTypes(
	Version, Arity, 
	Unused, unused,
	Module, moduleString, pprModule,
	Fixity(..), FixityDirection(..), StrictnessMark(..),
	NewOrData(..), IfaceFlavour(..), TopLevelFlag(..), RecFlag(..)
   ) where

#include "HsVersions.h"

import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Unused]{Unused}
%*									*
%************************************************************************

Used as a placeholder in types.

\begin{code}
type Unused = Void

unused :: Unused
unused = error "Unused is used!"
\end{code}


%************************************************************************
%*									*
\subsection[Arity]{Arity}
%*									*
%************************************************************************

\begin{code}
type Arity = Int
\end{code}


%************************************************************************
%*									*
\subsection[Version]{Module and identifier version numbers}
%*									*
%************************************************************************

\begin{code}
type Version = Int
\end{code}


%************************************************************************
%*									*
\subsection[Module]{The name of a module}
%*									*
%************************************************************************

\begin{code}
type Module   = FAST_STRING

moduleString :: Module -> String
moduleString mod = _UNPK_ mod

pprModule :: Module -> SDoc
pprModule m = ptext m
\end{code}

%************************************************************************
%*									*
\subsection[IfaceFlavour]{IfaceFlavour}
%*									*
%************************************************************************

The IfaceFlavour type is used mainly in an imported Name's Provenance
to say whether the name comes from a regular .hi file, or whether it comes
from a hand-written .hi-boot file.  This is important, because it has to be 
propagated.  Suppose

	C.hs imports B
	B.hs imports A
	A.hs imports C {-# SOURCE -#} ( f )

Then in A.hi we may mention C.f, in an inlining.  When compiling B we *must not* 
read C.f's details from C.hi, even if the latter happens to exist from an earlier
compilation run.  So we use the name "C!f" in A.hi, and when looking for an interface
file with details of C!f we look in C.hi-boot.  The "!" stuff is recorded in the
IfaceFlavour in the Name of C.f in A. 

Not particularly beautiful, but it works.

\begin{code}
data IfaceFlavour = HiFile		-- The interface was read from a standard interface file
		  | HiBootFile		-- ... or from a handwritten "hi-boot" interface file

instance Text IfaceFlavour where	-- Just used in debug prints of lex tokens
  showsPrec n HiFile     s = s
  showsPrec n HiBootFile s = "!" ++ s
\end{code}


%************************************************************************
%*									*
\subsection[Fixity]{Fixity info}
%*									*
%************************************************************************

\begin{code}
data Fixity = Fixity Int FixityDirection
data FixityDirection = InfixL | InfixR | InfixN 
		     deriving(Eq)

instance Outputable Fixity where
    ppr (Fixity prec dir) = hcat [ppr dir, space, int prec]

instance Outputable FixityDirection where
    ppr InfixL = ptext SLIT("infixl")
    ppr InfixR = ptext SLIT("infixr")
    ppr InfixN = ptext SLIT("infix")

instance Eq Fixity where		-- Used to determine if two fixities conflict
  (Fixity p1 dir1) == (Fixity p2 dir2) = p1==p2 && dir1 == dir2
\end{code}


%************************************************************************
%*									*
\subsection[NewType/DataType]{NewType/DataType flag}
%*									*
%************************************************************************

\begin{code}
data NewOrData
  = NewType  	-- "newtype Blah ..."
  | DataType 	-- "data Blah ..."
  | EnumType	-- Enumeration; all constructors are nullary
  deriving( Eq )	-- Needed because Demand derives Eq
\end{code}

The @RecFlag@ tells whether the thing is part of a recursive group or not.


%************************************************************************
%*									*
\subsection[Top-level/local]{Top-level/not-top level flag}
%*									*
%************************************************************************

\begin{code}
data TopLevelFlag
  = TopLevel
  | NotTopLevel
\end{code}


%************************************************************************
%*									*
\subsection[Top-level/local]{Top-level/not-top level flag}
%*									*
%************************************************************************

\begin{code} 
data RecFlag = Recursive 
	     | NonRecursive
\end{code}

%************************************************************************
%*									*
\subsection{Strictness indication}
%*									*
%************************************************************************

\begin{code}
data StrictnessMark = MarkedStrict
		    | NotMarkedStrict
\end{code}
