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

	OccInfo(..), seqOccInfo, isFragileOcc, isDeadOcc, isLoopBreaker,

	InsideLam, insideLam, notInsideLam,
	OneBranch, oneBranch, notOneBranch,

        EP(..),

	StrictnessMark(..), isMarkedUnboxed, isMarkedStrict,

	CompilerPhase, pprPhase, 
	Activation(..), isActive, isNeverActive, isAlwaysActive
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

bumpVersion :: Bool -> Version -> Version 
-- Bump if the predicate (typically equality between old and new) is false
bumpVersion False v = v+1
bumpVersion True  v = v

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
\subsection[Generic]{Generic flag}
%*									*
%************************************************************************

This is the "Embedding-Projection pair" datatype, it contains 
two pieces of code (normally either RenamedHsExpr's or Id's)
If we have a such a pair (EP from to), the idea is that 'from' and 'to'
represents functions of type 

	from :: T -> Tring
	to   :: Tring -> T

And we should have 

	to (from x) = x

T and Tring are arbitrary, but typically T is the 'main' type while
Tring is the 'representation' type.  (This just helps us remember 
whether to use 'from' or 'to'.

\begin{code}
data EP a = EP { fromEP :: a,	-- :: T -> Tring
		 toEP   :: a }	-- :: Tring -> T
\end{code}

Embedding-projection pairs are used in several places:

First of all, each type constructor has an EP associated with it, the
code in EP converts (datatype T) from T to Tring and back again.

Secondly, when we are filling in Generic methods (in the typechecker, 
tcMethodBinds), we are constructing bimaps by induction on the structure
of the type of the method signature.


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

isDeadOcc :: OccInfo -> Bool
isDeadOcc IAmDead = True
isDeadOcc other	  = False

isFragileOcc :: OccInfo -> Bool
isFragileOcc (OneOcc _ _) = True
isFragileOcc other	  = False
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

%************************************************************************
%*									*
\subsection{Strictness indication}
%*									*
%************************************************************************

The strictness annotations on types in data type declarations
e.g. 	data T = MkT !Int !(Bool,Bool)

\begin{code}
data StrictnessMark
   = MarkedUserStrict	-- "!"  in a source decl
   | MarkedStrict	-- "!"  in an interface decl: strict but not unboxed
   | MarkedUnboxed	-- "!!" in an interface decl: unboxed 
   | NotMarkedStrict	-- No annotation at all
   deriving( Eq )

isMarkedUnboxed MarkedUnboxed = True
isMarkedUnboxed other	      = False

isMarkedStrict NotMarkedStrict = False
isMarkedStrict other	       = True	-- All others are strict

instance Outputable StrictnessMark where
  ppr MarkedUserStrict = ptext SLIT("!u")
  ppr MarkedStrict     = ptext SLIT("!")
  ppr MarkedUnboxed    = ptext SLIT("! !")
  ppr NotMarkedStrict  = empty
\end{code}


%************************************************************************
%*									*
\subsection{Activation}
%*									*
%************************************************************************

When a rule or inlining is active

\begin{code}
type CompilerPhase = Int	-- Compilation phase
				-- Phases decrease towards zero
				-- Zero is the last phase

pprPhase :: CompilerPhase -> SDoc
pprPhase n = brackets (int n)

data Activation = NeverActive
		| AlwaysActive
		| ActiveAfter CompilerPhase	-- Active in this phase and later
		deriving( Eq )			-- Eq used in comparing rules in HsDecls

instance Outputable Activation where
   ppr AlwaysActive    = empty		-- The default
   ppr (ActiveAfter n) = pprPhase n
   ppr NeverActive     = ptext SLIT("NEVER")
    
isActive :: CompilerPhase -> Activation -> Bool
isActive p NeverActive     = False
isActive p AlwaysActive    = True
isActive p (ActiveAfter n) = p <= n

isNeverActive, isAlwaysActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive act	  = False

isAlwaysActive AlwaysActive = True
isAlwaysActive other	    = False
\end{code}
