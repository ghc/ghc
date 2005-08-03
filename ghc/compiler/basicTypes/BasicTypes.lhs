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
	Version, bumpVersion, initialVersion,

	Arity, 
	
	DeprecTxt,

	Fixity(..), FixityDirection(..),
	defaultFixity, maxPrecedence, 
	negateFixity, funTyFixity,
	compareFixity,

	IPName(..), ipNameName, mapIPName,

	RecFlag(..), isRec, isNonRec, boolToRecFlag,

	TopLevelFlag(..), isTopLevel, isNotTopLevel,

	Boxity(..), isBoxed, 

	TupCon(..), tupleParens,

	OccInfo(..), seqOccInfo, isFragileOcc, isOneOcc, 
	isDeadOcc, isLoopBreaker,

	InsideLam, insideLam, notInsideLam,
	OneBranch, oneBranch, notOneBranch,
	InterestingCxt,

        EP(..),

	StrictnessMark(..), isMarkedUnboxed, isMarkedStrict,

	CompilerPhase, 
	Activation(..), isActive, isNeverActive, isAlwaysActive,

	SuccessFlag(..), succeeded, failed, successIf
   ) where

#include "HsVersions.h"

import FastString( FastString )
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

bumpVersion :: Version -> Version 
bumpVersion v = v+1

initialVersion :: Version
initialVersion = 1
\end{code}

%************************************************************************
%*									*
		Deprecations
%*									*
%************************************************************************


\begin{code}
type DeprecTxt = FastString	-- reason/explanation for deprecation
\end{code}

%************************************************************************
%*									*
\subsection{Implicit parameter identity}
%*									*
%************************************************************************

The @IPName@ type is here because it is used in TypeRep (i.e. very
early in the hierarchy), but also in HsSyn.

\begin{code}
data IPName name
  = Dupable   name	-- ?x: you can freely duplicate this implicit parameter
  | Linear name		-- %x: you must use the splitting function to duplicate it
  deriving( Eq, Ord )	-- Ord is used in the IP name cache finite map
			--	(used in HscTypes.OrigIParamCache)


ipNameName :: IPName name -> name
ipNameName (Dupable n) = n
ipNameName (Linear  n) = n

mapIPName :: (a->b) -> IPName a -> IPName b
mapIPName f (Dupable n) = Dupable (f n)
mapIPName f (Linear  n) = Linear  (f n)

instance Outputable name => Outputable (IPName name) where
    ppr (Dupable n) = char '?' <> ppr n -- Ordinary implicit parameters
    ppr (Linear  n) = char '%' <> ppr n -- Splittable implicit parameters
\end{code}


%************************************************************************
%*									*
\subsection[Fixity]{Fixity info}
%*									*
%************************************************************************

\begin{code}
------------------------
data Fixity = Fixity Int FixityDirection

instance Outputable Fixity where
    ppr (Fixity prec dir) = hcat [ppr dir, space, int prec]

instance Eq Fixity where		-- Used to determine if two fixities conflict
  (Fixity p1 dir1) == (Fixity p2 dir2) = p1==p2 && dir1 == dir2

------------------------
data FixityDirection = InfixL | InfixR | InfixN 
		     deriving(Eq)

instance Outputable FixityDirection where
    ppr InfixL = ptext SLIT("infixl")
    ppr InfixR = ptext SLIT("infixr")
    ppr InfixN = ptext SLIT("infix")

------------------------
maxPrecedence = (9::Int)
defaultFixity = Fixity maxPrecedence InfixL

negateFixity, funTyFixity :: Fixity
-- Wired-in fixities
negateFixity = Fixity 6 InfixL 	-- Fixity of unary negate
funTyFixity  = Fixity 0	InfixR	-- Fixity of '->'
\end{code}

Consider

\begin{verbatim}
	a `op1` b `op2` c
\end{verbatim}
@(compareFixity op1 op2)@ tells which way to arrange appication, or
whether there's an error.

\begin{code}
compareFixity :: Fixity -> Fixity
	      -> (Bool,		-- Error please
		  Bool)		-- Associate to the right: a op1 (b op2 c)
compareFixity (Fixity prec1 dir1) (Fixity prec2 dir2)
  = case prec1 `compare` prec2 of
	GT -> left
	LT -> right
	EQ -> case (dir1, dir2) of
			(InfixR, InfixR) -> right
			(InfixL, InfixL) -> left
			_		 -> error_please
  where
    right	 = (False, True)
    left         = (False, False)
    error_please = (True,  False)
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

instance Outputable TopLevelFlag where
  ppr TopLevel    = ptext SLIT("<TopLevel>")
  ppr NotTopLevel = ptext SLIT("<NotTopLevel>")
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
\end{code}


%************************************************************************
%*									*
\subsection[Recursive/Non-Recursive]{Recursive/Non-Recursive flag}
%*									*
%************************************************************************

\begin{code} 
data RecFlag = Recursive 
	     | NonRecursive
	     deriving( Eq )

isRec :: RecFlag -> Bool
isRec Recursive    = True
isRec NonRecursive = False

isNonRec :: RecFlag -> Bool
isNonRec Recursive    = False
isNonRec NonRecursive = True

boolToRecFlag :: Bool -> RecFlag
boolToRecFlag True  = Recursive
boolToRecFlag False = NonRecursive

instance Outputable RecFlag where
  ppr Recursive    = ptext SLIT("Recursive")
  ppr NonRecursive = ptext SLIT("NonRecursive")
\end{code}

%************************************************************************
%*									*
		Tuples
%*									*
%************************************************************************

\begin{code}
data TupCon = TupCon Boxity Arity

instance Eq TupCon where
  (TupCon b1 a1) == (TupCon b2 a2) = b1==b2 && a1==a2
   
tupleParens :: Boxity -> SDoc -> SDoc
tupleParens Boxed   p = parens p
tupleParens Unboxed p = ptext SLIT("(#") <+> p <+> ptext SLIT("#)")
\end{code}

%************************************************************************
%*									*
\subsection[Generic]{Generic flag}
%*									*
%************************************************************************

This is the "Embedding-Projection pair" datatype, it contains 
two pieces of code (normally either RenamedExpr's or Id's)
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

  | OneOcc !InsideLam
 	   !OneBranch
	   !InterestingCxt

  | IAmALoopBreaker	-- Used by the occurrence analyser to mark loop-breakers
			-- in a group of recursive definitions

seqOccInfo :: OccInfo -> ()
seqOccInfo occ = occ `seq` ()

-----------------
type InterestingCxt = Bool	-- True <=> Function: is applied
				--	    Data value: scrutinised by a case with
				--			at least one non-DEFAULT branch

-----------------
type InsideLam = Bool	-- True <=> Occurs inside a non-linear lambda
			-- Substituting a redex for this occurrence is
			-- dangerous because it might duplicate work.
insideLam    = True
notInsideLam = False

-----------------
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

isOneOcc (OneOcc _ _ _) = True
isOneOcc other	        = False

isFragileOcc :: OccInfo -> Bool
isFragileOcc (OneOcc _ _ _) = True
isFragileOcc other	    = False
\end{code}

\begin{code}
instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoOccInfo  			  	  = empty
  ppr IAmALoopBreaker 				  = ptext SLIT("LoopBreaker")
  ppr IAmDead					  = ptext SLIT("Dead")
  ppr (OneOcc inside_lam one_branch int_cxt)
	= ptext SLIT("Once") <> pp_lam <> pp_br <> pp_args
	where
	  pp_lam | inside_lam = char 'L'
		 | otherwise  = empty
	  pp_br  | one_branch = empty
		 | otherwise  = char '*'
	  pp_args | int_cxt   = char '!'
		  | otherwise = empty

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
data StrictnessMark	-- Used in interface decls only
   = MarkedStrict	
   | MarkedUnboxed	
   | NotMarkedStrict	
   deriving( Eq )

isMarkedUnboxed MarkedUnboxed = True
isMarkedUnboxed other	      = False

isMarkedStrict NotMarkedStrict = False
isMarkedStrict other	       = True	-- All others are strict

instance Outputable StrictnessMark where
  ppr MarkedStrict     = ptext SLIT("!")
  ppr MarkedUnboxed    = ptext SLIT("!!")
  ppr NotMarkedStrict  = ptext SLIT("_")
\end{code}


%************************************************************************
%*									*
\subsection{Success flag}
%*									*
%************************************************************************

\begin{code}
data SuccessFlag = Succeeded | Failed

successIf :: Bool -> SuccessFlag
successIf True  = Succeeded
successIf False = Failed

succeeded, failed :: SuccessFlag -> Bool
succeeded Succeeded = True
succeeded Failed    = False

failed Succeeded = False
failed Failed    = True
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

data Activation = NeverActive
		| AlwaysActive
		| ActiveBefore CompilerPhase	-- Active only *before* this phase
		| ActiveAfter CompilerPhase	-- Active in this phase and later
		deriving( Eq )			-- Eq used in comparing rules in HsDecls

instance Outputable Activation where
   ppr AlwaysActive     = empty		-- The default
   ppr (ActiveBefore n) = brackets (char '~' <> int n)
   ppr (ActiveAfter n)  = brackets (int n)
   ppr NeverActive      = ptext SLIT("NEVER")
    
isActive :: CompilerPhase -> Activation -> Bool
isActive p NeverActive      = False
isActive p AlwaysActive     = True
isActive p (ActiveAfter n)  = p <= n
isActive p (ActiveBefore n) = p >  n

isNeverActive, isAlwaysActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive act	  = False

isAlwaysActive AlwaysActive = True
isAlwaysActive other	    = False
\end{code}

