%
% (c) The GRASP/AQUA Project, Glasgow University, 1997
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
#include "HsVersions.h"

module BasicTypes(
	SYN_IE(Version), SYN_IE(Arity),
	SYN_IE(Module), moduleString, pprModule,
	Fixity(..), FixityDirection(..),
	NewOrData(..)
   ) where

IMP_Ubiq()

import Pretty
import Outputable

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

pprModule :: PprStyle -> Module -> Doc
pprModule sty m = ptext m
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
    ppr sty (Fixity prec dir) = hcat [ppr sty dir, space, int prec]

instance Outputable FixityDirection where
    ppr sty InfixL = ptext SLIT("infixl")
    ppr sty InfixR = ptext SLIT("infixr")
    ppr sty InfixN = ptext SLIT("infix")

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
  = NewType	    -- "newtype Blah ..."
  | DataType	    -- "data Blah ..."
  deriving( Eq )
\end{code}
