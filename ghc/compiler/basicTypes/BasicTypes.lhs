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
	Fixity(..), FixityDirection(..),
	defaultFixity, maxPrecedence, negateFixity, negatePrecedence,
	NewOrData(..), 
	RecFlag(..), isRec, isNonRec,
	TopLevelFlag(..), isTopLevel, isNotTopLevel
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
type Unused = ()

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

maxPrecedence = (9::Int)
defaultFixity = Fixity maxPrecedence InfixL

negateFixity :: Fixity
negateFixity     = Fixity negatePrecedence InfixL  	-- Precedence of unary negate is wired in as infixl 6!

negatePrecedence :: Int
negatePrecedence = 6
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

%************************************************************************
%*									*
\subsection[Top-level/local]{Top-level/not-top level flag}
%*									*
%************************************************************************

\begin{code}
data TopLevelFlag
  = TopLevel
  | NotTopLevel

isTopLevel, isNotTopLevel :: TopLevelFlag -> Bool

isNotTopLevel NotTopLevel = True
isNotTopLevel TopLevel    = False

isTopLevel TopLevel	= True
isTopLevel NotTopLevel  = False
\end{code}

%************************************************************************
%*									*
\subsection[Recursive/Non-Recursive]{Recursive/Non-Recursive flag}
%*									*
%************************************************************************

\begin{code} 
data RecFlag = Recursive 
	     | NonRecursive

isRec :: RecFlag -> Bool
isRec Recursive    = True
isRec NonRecursive = False

isNonRec :: RecFlag -> Bool
isNonRec Recursive    = False
isNonRec NonRecursive = True
\end{code}
