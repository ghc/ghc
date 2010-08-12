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
{-# LANGUAGE DeriveDataTypeable #-}

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

	OccInfo(..), seqOccInfo, zapFragileOcc, isOneOcc, 
	isDeadOcc, isLoopBreaker, isNonRuleLoopBreaker, isNoOcc,
        nonRuleLoopBreaker,

	InsideLam, insideLam, notInsideLam,
	OneBranch, oneBranch, notOneBranch,
	InterestingCxt,

        EP(..),

	HsBang(..), isBanged, isMarkedUnboxed, 
        StrictnessMark(..), isMarkedStrict,

	DefMethSpec(..),

	CompilerPhase, 
	Activation(..), isActive, isNeverActive, isAlwaysActive, isEarlyActive,
        RuleMatchInfo(..), isConLike, isFunLike, 
        InlinePragma(..), defaultInlinePragma, alwaysInlinePragma, neverInlinePragma, dfunInlinePragma,
	isDefaultInlinePragma, isInlinePragma, inlinePragmaSat,
        inlinePragmaActivation, inlinePragmaRuleMatchInfo,
        setInlinePragmaActivation, setInlinePragmaRuleMatchInfo,

	SuccessFlag(..), succeeded, failed, successIf
   ) where

import FastString
import Outputable

import Data.Data hiding (Fixity)
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
    deriving (Eq, Ord, Data, Typeable)

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
    deriving (Eq, Data, Typeable)

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
  deriving( Eq, Ord, Data, Typeable )
  -- Ord is used in the IP name cache finite map
  -- (used in HscTypes.OrigIParamCache)

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
  deriving (Data, Typeable)

instance Outputable Fixity where
    ppr (Fixity prec dir) = hcat [ppr dir, space, int prec]

instance Eq Fixity where		-- Used to determine if two fixities conflict
  (Fixity p1 dir1) == (Fixity p2 dir2) = p1==p2 && dir1 == dir2

------------------------
data FixityDirection = InfixL | InfixR | InfixN 
		     deriving (Eq, Data, Typeable)

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
  deriving( Eq, Data, Typeable )

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
	     deriving( Eq, Data, Typeable )

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

nonRuleLoopBreaker :: OccInfo
nonRuleLoopBreaker = IAmALoopBreaker False

isDeadOcc :: OccInfo -> Bool
isDeadOcc IAmDead = True
isDeadOcc _       = False

isOneOcc :: OccInfo -> Bool
isOneOcc (OneOcc {}) = True
isOneOcc _           = False

zapFragileOcc :: OccInfo -> OccInfo
zapFragileOcc (OneOcc {}) = NoOccInfo
zapFragileOcc occ         = occ
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
		Strictness indication
%*									*
%************************************************************************

The strictness annotations on types in data type declarations
e.g. 	data T = MkT !Int !(Bool,Bool)

\begin{code}
-------------------------
-- HsBang describes what the *programmer* wrote
-- This info is retained in the DataCon.dcStrictMarks field
data HsBang = HsNoBang	

	    | HsStrict	

	    | HsUnpack	       -- {-# UNPACK #-} ! (GHC extension, meaning "unbox")

	    | HsUnpackFailed   -- An UNPACK pragma that we could not make 
	      		       -- use of, because the type isn't unboxable; 
                               -- equivalant to HsStrict except for checkValidDataCon
  deriving (Eq, Data, Typeable)

instance Outputable HsBang where
    ppr HsNoBang       = empty
    ppr HsStrict       = char '!'
    ppr HsUnpack       = ptext (sLit "{-# UNPACK #-} !")
    ppr HsUnpackFailed = ptext (sLit "{-# UNPACK (failed) #-} !")

isBanged :: HsBang -> Bool
isBanged HsNoBang = False
isBanged _        = True

isMarkedUnboxed :: HsBang -> Bool
isMarkedUnboxed HsUnpack = True
isMarkedUnboxed _        = False

-------------------------
-- StrictnessMark is internal only, used to indicate strictness 
-- of the DataCon *worker* fields
data StrictnessMark = MarkedStrict | NotMarkedStrict	

instance Outputable StrictnessMark where
  ppr MarkedStrict     = ptext (sLit "!")
  ppr NotMarkedStrict  = empty

isMarkedStrict :: StrictnessMark -> Bool
isMarkedStrict NotMarkedStrict = False
isMarkedStrict _               = True   -- All others are strict
\end{code}


%************************************************************************
%*									*
		Default method specfication
%*									*
%************************************************************************

The DefMethSpec enumeration just indicates what sort of default method
is used for a class. It is generated from source code, and present in 
interface files; it is converted to Class.DefMeth before begin put in a 
Class object.

\begin{code}
data DefMethSpec = NoDM        -- No default method
                 | VanillaDM   -- Default method given with polymorphic code
                 | GenericDM   -- Default method given with generic code

instance Outputable DefMethSpec where
  ppr NoDM      = empty
  ppr VanillaDM = ptext (sLit "{- Has default method -}")
  ppr GenericDM = ptext (sLit "{- Has generic default method -}")
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
		deriving( Eq, Data, Typeable )	-- Eq used in comparing rules in HsDecls

data RuleMatchInfo = ConLike 			-- See Note [CONLIKE pragma]
                   | FunLike
                   deriving( Eq, Data, Typeable )

data InlinePragma  	     -- Note [InlinePragma]
  = InlinePragma
      { inl_inline :: Bool           -- True <=> INLINE, 
      		      		     -- False <=> no pragma at all, or NOINLINE

      , inl_sat    :: Maybe Arity    -- Just n <=> Inline only when applied to n 
      		      	    	     --            explicit (non-type, non-dictionary) args
				     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the 
                                     --   number of implicit, dictionary arguments when making
			             --   the InlineRule, and don't look at inl_sat further

      , inl_act    :: Activation     -- Says during which phases inlining is allowed

      , inl_rule   :: RuleMatchInfo  -- Should the function be treated like a constructor?
    } deriving( Eq, Data, Typeable )
\end{code}

Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~
This data type mirrors what you can write in an INLINE or NOINLINE pragma in 
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = False
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so 
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = True, then the Id should have an InlineRule unfolding.

Note [CONLIKE pragma]
~~~~~~~~~~~~~~~~~~~~~
The ConLike constructor of a RuleMatchInfo is aimed at the following.
Consider first
    {-# RULE "r/cons" forall a as. r (a:as) = f (a+1) #-}
    g b bs = let x = b:bs in ..x...x...(r x)...
Now, the rule applies to the (r x) term, because GHC "looks through" 
the definition of 'x' to see that it is (b:bs).

Now consider
    {-# RULE "r/f" forall v. r (f v) = f (v+1) #-}
    g v = let x = f v in ..x...x...(r x)...
Normally the (r x) would *not* match the rule, because GHC would be
scared about duplicating the redex (f v), so it does not "look
through" the bindings.  

However the CONLIKE modifier says to treat 'f' like a constructor in
this situation, and "look through" the unfolding for x.  So (r x)
fires, yielding (f (v+1)).

This is all controlled with a user-visible pragma:
     {-# NOINLINE CONLIKE [1] f #-}

The main effects of CONLIKE are:

    - The occurrence analyser (OccAnal) and simplifier (Simplify) treat
      CONLIKE thing like constructors, by ANF-ing them

    - New function coreUtils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in Rules.lhs.

\begin{code}
isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _            = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _            = False

defaultInlinePragma, alwaysInlinePragma, neverInlinePragma, dfunInlinePragma
  :: InlinePragma
defaultInlinePragma = InlinePragma { inl_act = AlwaysActive
                                   , inl_rule = FunLike
                                   , inl_inline = False
                                   , inl_sat = Nothing }

alwaysInlinePragma = defaultInlinePragma { inl_inline = True }
neverInlinePragma  = defaultInlinePragma { inl_act    = NeverActive }

-- A DFun has an always-active inline activation so that 
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
dfunInlinePragma   = defaultInlinePragma { inl_act  = AlwaysActive
                                         , inl_rule = ConLike }

isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = not inline && isAlwaysActive activation && isFunLike match_info

isInlinePragma :: InlinePragma -> Bool
isInlinePragma prag = inl_inline prag

inlinePragmaSat :: InlinePragma -> Maybe Arity
inlinePragmaSat = inl_sat

inlinePragmaActivation :: InlinePragma -> Activation
inlinePragmaActivation (InlinePragma { inl_act = activation }) = activation

inlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo
inlinePragmaRuleMatchInfo (InlinePragma { inl_rule = info }) = info

setInlinePragmaActivation :: InlinePragma -> Activation -> InlinePragma
setInlinePragmaActivation prag activation = prag { inl_act = activation }

setInlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo -> InlinePragma
setInlinePragmaRuleMatchInfo prag info = prag { inl_rule = info }

instance Outputable Activation where
   ppr AlwaysActive     = brackets (ptext (sLit "ALWAYS"))
   ppr NeverActive      = brackets (ptext (sLit "NEVER"))
   ppr (ActiveBefore n) = brackets (char '~' <> int n)
   ppr (ActiveAfter n)  = brackets (int n)

instance Outputable RuleMatchInfo where
   ppr ConLike = ptext (sLit "CONLIKE")
   ppr FunLike = ptext (sLit "FUNLIKE")

instance Outputable InlinePragma where
  ppr (InlinePragma { inl_inline = inline, inl_act = activation
                    , inl_rule = info, inl_sat = mb_arity })
    = pp_inl_act (inline, activation) <+> pp_sat <+> pp_info 
    where
      pp_inl_act (False, AlwaysActive)  = empty	-- defaultInlinePragma
      pp_inl_act (False, NeverActive)   = ptext (sLit "NOINLINE")
      pp_inl_act (False, act)           = ptext (sLit "NOINLINE") <> ppr act
      pp_inl_act (True,  AlwaysActive)  = ptext (sLit "INLINE")
      pp_inl_act (True,  act)           = ptext (sLit "INLINE") <> ppr act

      pp_sat | Just ar <- mb_arity = parens (ptext (sLit "sat-args=") <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info

isActive :: CompilerPhase -> Activation -> Bool
isActive _ NeverActive      = False
isActive _ AlwaysActive     = True
isActive p (ActiveAfter n)  = p <= n
isActive p (ActiveBefore n) = p >  n

isNeverActive, isAlwaysActive, isEarlyActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive _           = False

isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

isEarlyActive AlwaysActive      = True
isEarlyActive (ActiveBefore {}) = True
isEarlyActive _		        = False
\end{code}

