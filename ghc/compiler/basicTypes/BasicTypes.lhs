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
	Version, bumpVersion, initialVersion, bogusVersion,

	Arity, 

	Unused, unused,

	Fixity(..), FixityDirection(..),
	defaultFixity, maxPrecedence, negateFixity, negatePrecedence,

	NewOrData(..), 

	RecFlag(..), isRec, isNonRec,

	TopLevelFlag(..), isTopLevel, isNotTopLevel,

	Boxity(..), isBoxed, tupleParens,

	OccInfo(..), seqOccInfo, isFragileOccInfo, isLoopBreaker,

	InsideLam, insideLam, notInsideLam,
	OneBranch, oneBranch, notOneBranch

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

bogusVersion :: Version	-- Shouldn't look at these
bogusVersion = error "bogusVersion"

bumpVersion :: Version -> Version 
bumpVersion v = v+1

initialVersion :: Version
initialVersion = 1
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
\subsection[Top-level/local]{Top-level/not-top level flag}
%*									*
%************************************************************************

\begin{code}
data Boxity
  = Boxed
  | Unboxed
  deriving( Eq )

isBoxed :: Boxity -> Bool
isBoxed Boxed   = True
isBoxed Unboxed = False

tupleParens :: Boxity -> SDoc -> SDoc
tupleParens Boxed   p = parens p
tupleParens Unboxed p = ptext SLIT("(#") <+> p <+> ptext SLIT("#)")
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


%************************************************************************
%*									*
\subsection{Occurrence information}
%*									*
%************************************************************************

This data type is used exclusively by the simplifier, but it appears in a
SubstResult, which is currently defined in VarEnv, which is pretty near
the base of the module hierarchy.  So it seemed simpler to put the
defn of OccInfo here, safely at the bottom

\begin{code}
data OccInfo 
  = NoOccInfo

  | IAmDead		-- Marks unused variables.  Sometimes useful for
			-- lambda and case-bound variables.

  | OneOcc InsideLam

 	   OneBranch

  | IAmALoopBreaker	-- Used by the occurrence analyser to mark loop-breakers
			-- in a group of recursive definitions

seqOccInfo :: OccInfo -> ()
seqOccInfo (OneOcc in_lam once) = in_lam `seq` once `seq` ()
seqOccInfo occ			= ()

type InsideLam = Bool	-- True <=> Occurs inside a non-linear lambda
			-- Substituting a redex for this occurrence is
			-- dangerous because it might duplicate work.
insideLam    = True
notInsideLam = False

type OneBranch = Bool	-- True <=> Occurs in only one case branch
			--	so no code-duplication issue to worry about
oneBranch    = True
notOneBranch = False

isLoopBreaker :: OccInfo -> Bool
isLoopBreaker IAmALoopBreaker = True
isLoopBreaker other	      = False

isFragileOccInfo :: OccInfo -> Bool
isFragileOccInfo (OneOcc _ _) = True
isFragileOccInfo other	      = False
\end{code}

\begin{code}
instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoOccInfo  			  	  = empty
  ppr IAmALoopBreaker 				  = ptext SLIT("_Kx")
  ppr IAmDead					  = ptext SLIT("_Kd")
  ppr (OneOcc inside_lam one_branch) | inside_lam = ptext SLIT("_Kl")
				     | one_branch = ptext SLIT("_Ks")
				     | otherwise  = ptext SLIT("_Ks*")

instance Show OccInfo where
  showsPrec p occ = showsPrecSDoc p (ppr occ)
\end{code}

