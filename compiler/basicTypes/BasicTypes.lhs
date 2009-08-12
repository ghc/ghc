%
% (c) The University of Glasgow 2006
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

    FunctionOrData(..),
	
	WarningTxt(..),

	Fixity(..), FixityDirection(..),
	defaultFixity, maxPrecedence, 
	negateFixity, funTyFixity,
	compareFixity,

	IPName(..), ipNameName, mapIPName,

	RecFlag(..), isRec, isNonRec, boolToRecFlag,

	RuleName,

	TopLevelFlag(..), isTopLevel, isNotTopLevel,

	OverlapFlag(..), 

	Boxity(..), isBoxed, 

	TupCon(..), tupleParens,

	OccInfo(..), seqOccInfo, isFragileOcc, isOneOcc, 
	isDeadOcc, isLoopBreaker, isNonRuleLoopBreaker, isNoOcc,

	InsideLam, insideLam, notInsideLam,
	OneBranch, oneBranch, notOneBranch,
	InterestingCxt,

        EP(..),

	StrictnessMark(..), isMarkedUnboxed, isMarkedStrict,

	CompilerPhase, 
	Activation(..), isActive, isNeverActive, isAlwaysActive,
        RuleMatchInfo(..), isConLike, isFunLike,
        InlinePragma(..), defaultInlinePragma, isDefaultInlinePragma,
        inlinePragmaActivation, inlinePragmaRuleMatchInfo,
        setInlinePragmaActivation, setInlinePragmaRuleMatchInfo,
	InlineSpec(..), defaultInlineSpec, alwaysInlineSpec, neverInlineSpec,

	SuccessFlag(..), succeeded, failed, successIf
   ) where

import FastString
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
\subsection[FunctionOrData]{FunctionOrData}
%*									*
%************************************************************************

\begin{code}
data FunctionOrData = IsFunction | IsData
    deriving (Eq, Ord)

instance Outputable FunctionOrData where
    ppr IsFunction = text "(function)"
    ppr IsData     = text "(data)"
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
-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt = WarningTxt [FastString]
                | DeprecatedTxt [FastString]
    deriving Eq

instance Outputable WarningTxt where
    ppr (WarningTxt    ws) = doubleQuotes (vcat (map ftext ws))
    ppr (DeprecatedTxt ds) = text "Deprecated:" <+>
                             doubleQuotes (vcat (map ftext ds))
\end{code}

%************************************************************************
%*									*
\subsection{Implicit parameter identity}
%*									*
%************************************************************************

The @IPName@ type is here because it is used in TypeRep (i.e. very
early in the hierarchy), but also in HsSyn.

\begin{code}
newtype IPName name = IPName name	-- ?x
  deriving( Eq, Ord )	-- Ord is used in the IP name cache finite map
			--	(used in HscTypes.OrigIParamCache)

ipNameName :: IPName name -> name
ipNameName (IPName n) = n

mapIPName :: (a->b) -> IPName a -> IPName b
mapIPName f (IPName n) = IPName (f n)

instance Outputable name => Outputable (IPName name) where
    ppr (IPName n) = char '?' <> ppr n -- Ordinary implicit parameters
\end{code}

%************************************************************************
%*									*
		Rules
%*									*
%************************************************************************

\begin{code}
type RuleName = FastString
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
    ppr InfixL = ptext (sLit "infixl")
    ppr InfixR = ptext (sLit "infixr")
    ppr InfixN = ptext (sLit "infix")

------------------------
maxPrecedence :: Int
maxPrecedence = 9
defaultFixity :: Fixity
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
  ppr TopLevel    = ptext (sLit "<TopLevel>")
  ppr NotTopLevel = ptext (sLit "<NotTopLevel>")
\end{code}


%************************************************************************
%*									*
		Top-level/not-top level flag
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
		Recursive/Non-Recursive flag
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
  ppr Recursive    = ptext (sLit "Recursive")
  ppr NonRecursive = ptext (sLit "NonRecursive")
\end{code}

%************************************************************************
%*									*
		Instance overlap flag
%*									*
%************************************************************************

\begin{code}
data OverlapFlag
  = NoOverlap	-- This instance must not overlap another

  | OverlapOk	-- Silently ignore this instance if you find a 
		-- more specific one that matches the constraint
		-- you are trying to resolve
		--
		-- Example: constraint (Foo [Int])
		-- 	    instances  (Foo [Int])
		--		       (Foo [a])	OverlapOk
		-- Since the second instance has the OverlapOk flag,
		-- the first instance will be chosen (otherwise 
		-- its ambiguous which to choose)

  | Incoherent	-- Like OverlapOk, but also ignore this instance 
		-- if it doesn't match the constraint you are
		-- trying to resolve, but could match if the type variables
		-- in the constraint were instantiated
		--
		-- Example: constraint (Foo [b])
		--	    instances  (Foo [Int])	Incoherent
		--		       (Foo [a])
		-- Without the Incoherent flag, we'd complain that
		-- instantiating 'b' would change which instance 
		-- was chosen
  deriving( Eq )

instance Outputable OverlapFlag where
   ppr NoOverlap  = empty
   ppr OverlapOk  = ptext (sLit "[overlap ok]")
   ppr Incoherent = ptext (sLit "[incoherent]")

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
tupleParens Unboxed p = ptext (sLit "(#") <+> p <+> ptext (sLit "#)")
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
-- | Identifier occurrence information
data OccInfo 
  = NoOccInfo		-- ^ There are many occurrences, or unknown occurences

  | IAmDead		-- ^ Marks unused variables.  Sometimes useful for
			-- lambda and case-bound variables.

  | OneOcc
	!InsideLam
 	!OneBranch
	!InterestingCxt -- ^ Occurs exactly once, not inside a rule

  -- | This identifier breaks a loop of mutually recursive functions. The field
  -- marks whether it is only a loop breaker due to a reference in a rule
  | IAmALoopBreaker	-- Note [LoopBreaker OccInfo]
	!RulesOnly	-- True <=> This is a weak or rules-only loop breaker
			--  	    See OccurAnal Note [Weak loop breakers]

type RulesOnly = Bool
\end{code}

Note [LoopBreaker OccInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
An OccInfo of (IAmLoopBreaker False) is used by the occurrence 
analyser in two ways:
  (a) to mark loop-breakers in a group of recursive 
      definitions (hence the name)
  (b) to mark binders that must not be inlined in this phase
      (perhaps it has a NOINLINE pragma)
Things with (IAmLoopBreaker False) do not get an unfolding 
pinned on to them, so they are completely opaque.

See OccurAnal Note [Weak loop breakers] for (IAmLoopBreaker True).


\begin{code}
isNoOcc :: OccInfo -> Bool
isNoOcc NoOccInfo = True
isNoOcc _         = False

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
insideLam, notInsideLam :: InsideLam
insideLam    = True
notInsideLam = False

-----------------
type OneBranch = Bool	-- True <=> Occurs in only one case branch
			--	so no code-duplication issue to worry about
oneBranch, notOneBranch :: OneBranch
oneBranch    = True
notOneBranch = False

isLoopBreaker :: OccInfo -> Bool
isLoopBreaker (IAmALoopBreaker _) = True
isLoopBreaker _                   = False

isNonRuleLoopBreaker :: OccInfo -> Bool
isNonRuleLoopBreaker (IAmALoopBreaker False) = True   -- Loop-breaker that breaks a non-rule cycle
isNonRuleLoopBreaker _                       = False

isDeadOcc :: OccInfo -> Bool
isDeadOcc IAmDead = True
isDeadOcc _       = False

isOneOcc :: OccInfo -> Bool
isOneOcc (OneOcc _ _ _) = True
isOneOcc _              = False

isFragileOcc :: OccInfo -> Bool
isFragileOcc (OneOcc _ _ _) = True
isFragileOcc _              = False
\end{code}

\begin{code}
instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoOccInfo  	   = empty
  ppr (IAmALoopBreaker ro) = ptext (sLit "LoopBreaker") <> if ro then char '!' else empty
  ppr IAmDead		   = ptext (sLit "Dead")
  ppr (OneOcc inside_lam one_branch int_cxt)
	= ptext (sLit "Once") <> pp_lam <> pp_br <> pp_args
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

isMarkedUnboxed :: StrictnessMark -> Bool
isMarkedUnboxed MarkedUnboxed = True
isMarkedUnboxed _             = False

isMarkedStrict :: StrictnessMark -> Bool
isMarkedStrict NotMarkedStrict = False
isMarkedStrict _               = True   -- All others are strict

instance Outputable StrictnessMark where
  ppr MarkedStrict     = ptext (sLit "!")
  ppr MarkedUnboxed    = ptext (sLit "!!")
  ppr NotMarkedStrict  = ptext (sLit "_")
\end{code}


%************************************************************************
%*									*
\subsection{Success flag}
%*									*
%************************************************************************

\begin{code}
data SuccessFlag = Succeeded | Failed

instance Outputable SuccessFlag where
    ppr Succeeded = ptext (sLit "Succeeded")
    ppr Failed    = ptext (sLit "Failed")

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

data RuleMatchInfo = ConLike
                   | FunLike
                   deriving( Eq )

isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _            = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _            = False

data InlinePragma
  = InlinePragma
      Activation        -- Says during which phases inlining is allowed
      RuleMatchInfo     -- Should the function be treated like a constructor?
  deriving( Eq )

defaultInlinePragma :: InlinePragma
defaultInlinePragma = InlinePragma AlwaysActive FunLike

isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma activation match_info)
  = isAlwaysActive activation && isFunLike match_info

inlinePragmaActivation :: InlinePragma -> Activation
inlinePragmaActivation (InlinePragma activation _) = activation

inlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo
inlinePragmaRuleMatchInfo (InlinePragma _ info) = info

setInlinePragmaActivation :: InlinePragma -> Activation -> InlinePragma
setInlinePragmaActivation (InlinePragma _ info) activation
  = InlinePragma activation info

setInlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo -> InlinePragma
setInlinePragmaRuleMatchInfo (InlinePragma activation _) info
  = InlinePragma activation info

data InlineSpec
  = Inline
        InlinePragma
	Bool 		-- True  <=> INLINE
			-- False <=> NOINLINE
  deriving( Eq )

defaultInlineSpec :: InlineSpec
alwaysInlineSpec, neverInlineSpec :: RuleMatchInfo -> InlineSpec

defaultInlineSpec = Inline defaultInlinePragma False
                                                -- Inlining is OK, but not forced
alwaysInlineSpec match_info
                = Inline (InlinePragma AlwaysActive match_info) True
                                                -- INLINE always
neverInlineSpec match_info
                = Inline (InlinePragma NeverActive  match_info) False
                                                -- NOINLINE

instance Outputable Activation where
   ppr NeverActive      = ptext (sLit "NEVER")
   ppr AlwaysActive     = ptext (sLit "ALWAYS")
   ppr (ActiveBefore n) = brackets (char '~' <> int n)
   ppr (ActiveAfter n)  = brackets (int n)

instance Outputable RuleMatchInfo where
   ppr ConLike = ptext (sLit "CONLIKE")
   ppr FunLike = ptext (sLit "FUNLIKE")

instance Outputable InlinePragma where
  ppr (InlinePragma activation FunLike)
       = ppr activation
  ppr (InlinePragma activation match_info)
       = ppr match_info <+> ppr activation
    
instance Outputable InlineSpec where
   ppr (Inline (InlinePragma act match_info) is_inline)  
	| is_inline = ptext (sLit "INLINE")
                      <+> ppr_match_info
		      <+> case act of
			     AlwaysActive -> empty
			     _            -> ppr act
	| otherwise = ptext (sLit "NOINLINE")
                      <+> ppr_match_info
		      <+> case act of
			     NeverActive  -> empty
			     _            -> ppr act
     where
       ppr_match_info = if isFunLike match_info then empty else ppr match_info

isActive :: CompilerPhase -> Activation -> Bool
isActive _ NeverActive      = False
isActive _ AlwaysActive     = True
isActive p (ActiveAfter n)  = p <= n
isActive p (ActiveBefore n) = p >  n

isNeverActive, isAlwaysActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive _           = False

isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False
\end{code}

