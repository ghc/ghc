%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
module IdInfo (
	IdInfo,		-- Abstract

	noIdInfo,

	-- Arity
	ArityInfo(..),
	exactArity, atLeastArity, unknownArity,
	arityInfo, setArityInfo, ppArityInfo, arityLowerBound,

	-- Strictness
	StrictnessInfo(..),				-- Non-abstract
	mkStrictnessInfo,
	noStrictnessInfo, strictnessInfo,
	ppStrictnessInfo, setStrictnessInfo, 
	isBottomingStrictness, appIsBottom,

        -- Worker
        WorkerInfo, workerExists, 
        mkWorkerInfo, noWorkerInfo, workerInfo, setWorkerInfo,
        ppWorkerInfo,

	-- Unfolding
	unfoldingInfo, setUnfoldingInfo, 

	-- DemandInfo
	demandInfo, setDemandInfo, 

	-- Inline prags
	InlinePragInfo(..), OccInfo(..),
	inlinePragInfo, setInlinePragInfo, notInsideLambda,

	-- Specialisation
	IdSpecEnv, specInfo, setSpecInfo,

	-- Update
	UpdateInfo, UpdateSpec,
	mkUpdateInfo, updateInfo, updateInfoMaybe, ppUpdateInfo, setUpdateInfo,

	-- CAF info
	CafInfo(..), cafInfo, setCafInfo, ppCafInfo,

        -- Constructed Product Result Info
        CprInfo(..), cprInfo, setCprInfo, ppCprInfo, noCprInfo
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} CoreUnfold ( Unfolding, noUnfolding )
import {-# SOURCE #-} CoreSyn	 ( CoreExpr )

import Id               ( Id )
import SpecEnv	        ( SpecEnv, emptySpecEnv )
import Demand		( Demand,  isLazy, wwLazy, pprDemands )
import Outputable	

import Maybe            ( isJust )

\end{code}

An @IdInfo@ gives {\em optional} information about an @Id@.  If
present it never lies, but it may not be present, in which case there
is always a conservative assumption which can be made.

Two @Id@s may have different info even though they have the same
@Unique@ (and are hence the same @Id@); for example, one might lack
the properties attached to the other.

The @IdInfo@ gives information about the value, or definition, of the
@Id@.  It does {\em not} contain information about the @Id@'s usage
(except for @DemandInfo@? ToDo).

\begin{code}
data IdInfo
  = IdInfo {
	arityInfo :: ArityInfo,			-- Its arity
	demandInfo :: Demand,			-- Whether or not it is definitely demanded
	specInfo :: IdSpecEnv,			-- Specialisations of this function which exist
	strictnessInfo :: StrictnessInfo,	-- Strictness properties
        workerInfo :: WorkerInfo,               -- Pointer to Worker Function
	unfoldingInfo :: Unfolding,		-- Its unfolding
	updateInfo :: UpdateInfo,		-- Which args should be updated
	cafInfo :: CafInfo,
	cprInfo :: CprInfo,                     -- Function always constructs a product result
	inlinePragInfo :: !InlinePragInfo	-- Inline pragmas
    }
\end{code}

Setters

\begin{code}
setUpdateInfo	  ud info = info { updateInfo = ud }
setDemandInfo	  dd info = info { demandInfo = dd }
setStrictnessInfo st info = info { strictnessInfo = st }
setWorkerInfo     wk info = info { workerInfo = wk }
setSpecInfo 	  sp info = info { specInfo = sp }
setArityInfo	  ar info = info { arityInfo = ar  }
setInlinePragInfo pr info = info { inlinePragInfo = pr }
setUnfoldingInfo  uf info = info { unfoldingInfo = uf }
setCafInfo        cf info = info { cafInfo = cf }
setCprInfo        cp info = info { cprInfo = cp }
\end{code}


\begin{code}
noIdInfo = IdInfo {
		arityInfo	= UnknownArity,
		demandInfo	= wwLazy,
		specInfo	= emptySpecEnv,
		strictnessInfo	= NoStrictnessInfo,
		workerInfo	= noWorkerInfo,
		unfoldingInfo	= noUnfolding,
		updateInfo	= NoUpdateInfo,
		cafInfo		= MayHaveCafRefs,
		cprInfo		= NoCPRInfo,
		inlinePragInfo  = NoInlinePragInfo
	   }
\end{code}

%************************************************************************
%*									*
\subsection[arity-IdInfo]{Arity info about an @Id@}
%*									*
%************************************************************************

For locally-defined Ids, the code generator maintains its own notion
of their arities; so it should not be asking...	 (but other things
besides the code-generator need arity info!)

\begin{code}
data ArityInfo
  = UnknownArity	-- No idea
  | ArityExactly Int	-- Arity is exactly this
  | ArityAtLeast Int	-- Arity is this or greater

exactArity   = ArityExactly
atLeastArity = ArityAtLeast
unknownArity = UnknownArity

arityLowerBound :: ArityInfo -> Int
arityLowerBound UnknownArity     = 0
arityLowerBound (ArityAtLeast n) = n
arityLowerBound (ArityExactly n) = n


ppArityInfo UnknownArity	 = empty
ppArityInfo (ArityExactly arity) = hsep [ptext SLIT("__A"), int arity]
ppArityInfo (ArityAtLeast arity) = hsep [ptext SLIT("__AL"), int arity]
\end{code}

%************************************************************************
%*									*
\subsection{Inline-pragma information}
%*									*
%************************************************************************

\begin{code}
data InlinePragInfo
  = NoInlinePragInfo

  | IAmASpecPragmaId	-- Used for spec-pragma Ids; don't discard or inline

  | IWantToBeINLINEd	-- User INLINE pragma
  | IMustNotBeINLINEd	-- User NOINLINE pragma

  | IAmALoopBreaker	-- Used by the occurrence analyser to mark loop-breakers
			-- in a group of recursive definitions

  | ICanSafelyBeINLINEd	-- Used by the occurrence analyser to mark things
			-- that manifesly occur once, not inside SCCs, 
			-- not in constructor arguments

	OccInfo		-- Says whether the occurrence is inside a lambda
			--	If so, must only substitute WHNFs

	Bool		-- False <=> occurs in more than one case branch
			--	If so, there's a code-duplication issue

  | IAmDead		-- Marks unused variables.  Sometimes useful for
			-- lambda and case-bound variables.

  | IMustBeINLINEd	-- Absolutely must inline; used for PrimOps and
			-- constructors only.

instance Outputable InlinePragInfo where
  ppr NoInlinePragInfo  	= empty
  ppr IMustBeINLINEd    	= ptext SLIT("__UU")
  ppr IWantToBeINLINEd  	= ptext SLIT("__U")
  ppr IMustNotBeINLINEd 	= ptext SLIT("__Unot")
  ppr IAmALoopBreaker   	= ptext SLIT("__Ux")
  ppr IAmDead			= ptext SLIT("__Ud")
  ppr (ICanSafelyBeINLINEd InsideLam _) = ptext SLIT("__Ul")
  ppr (ICanSafelyBeINLINEd _ _) = ptext SLIT("__Us")
  ppr IAmASpecPragmaId 		= ptext SLIT("__US")

instance Show InlinePragInfo where
  showsPrec p prag = showsPrecSDoc p (ppr prag)
\end{code}

The @IMustNotBeDiscarded@ exists only to make Ids that are
on the *LHS* of bindings created by SPECIALISE pragmas; 
eg:		s = f Int d
The SpecPragmaId is never itself mentioned; it
exists solely so that the specialiser will find
the call to f, and make specialised version of it.
The SpecPragmaId binding is discarded by the specialiser
when it gathers up overloaded calls.
Meanwhile, it is not discarded as dead code.

\begin{code}
data OccInfo
  = StrictOcc		-- Occurs syntactically strictly;
			-- i.e. in a function position or case scrutinee

  | LazyOcc		-- Not syntactically strict (*even* that of a strict function)
			-- or in a case branch where there's more than one alternative

  | InsideLam		-- Inside a non-linear lambda (that is, a lambda which
			-- is sure to be instantiated only once).
			-- Substituting a redex for this occurrence is
			-- dangerous because it might duplicate work.

instance Outputable OccInfo where
  ppr StrictOcc = text "s"
  ppr LazyOcc   = empty
  ppr InsideLam = text "l"


notInsideLambda :: OccInfo -> Bool
notInsideLambda StrictOcc = True
notInsideLambda LazyOcc   = True
notInsideLambda InsideLam = False
\end{code}

%************************************************************************
%*									*
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
%*									*
%************************************************************************

A @IdSpecEnv@ holds details of an @Id@'s specialisations. 

\begin{code}
type IdSpecEnv = SpecEnv CoreExpr
\end{code}

For example, if \tr{f}'s @SpecEnv@ contains the mapping:
\begin{verbatim}
	[List a, b]  ===>  (\d -> f' a b)
\end{verbatim}
then when we find an application of f to matching types, we simply replace
it by the matching RHS:
\begin{verbatim}
	f (List Int) Bool ===>  (\d -> f' Int Bool)
\end{verbatim}
All the stuff about how many dictionaries to discard, and what types
to apply the specialised function to, are handled by the fact that the
SpecEnv contains a template for the result of the specialisation.

There is one more exciting case, which is dealt with in exactly the same
way.  If the specialised value is unboxed then it is lifted at its
definition site and unlifted at its uses.  For example:

	pi :: forall a. Num a => a

might have a specialisation

	[Int#] ===>  (case pi' of Lift pi# -> pi#)

where pi' :: Lift Int# is the specialised version of pi.


%************************************************************************
%*									*
\subsection[strictness-IdInfo]{Strictness info about an @Id@}
%*									*
%************************************************************************

We specify the strictness of a function by giving information about
each of the ``wrapper's'' arguments (see the description about
worker/wrapper-style transformations in the PJ/Launchbury paper on
unboxed types).

The list of @Demands@ specifies: (a)~the strictness properties of a
function's arguments; and (b)~the type signature of that worker (if it
exists); i.e. its calling convention.

Note that the existence of a worker function is now denoted by the Id's
workerInfo field.

\begin{code}
data StrictnessInfo
  = NoStrictnessInfo

  | StrictnessInfo [Demand] 
		   Bool		-- True <=> the function diverges regardless of its arguments
				-- Useful for "error" and other disguised variants thereof.  
				-- BUT NB: f = \x y. error "urk"
				-- 	   will have info  SI [SS] True
				-- but still (f) and (f 2) are not bot; only (f 3 2) is bot
\end{code}

\begin{code}
mkStrictnessInfo :: ([Demand], Bool) -> StrictnessInfo

mkStrictnessInfo (xs, is_bot)
  | all isLazy xs && not is_bot	= NoStrictnessInfo		-- Uninteresting
  | otherwise		        = StrictnessInfo xs is_bot

noStrictnessInfo       = NoStrictnessInfo

isBottomingStrictness (StrictnessInfo _ bot) = bot
isBottomingStrictness NoStrictnessInfo       = False

-- appIsBottom returns true if an application to n args would diverge
appIsBottom (StrictnessInfo ds bot)   n = bot && (n >= length ds)
appIsBottom  NoStrictnessInfo	      n	= False

ppStrictnessInfo NoStrictnessInfo = empty
ppStrictnessInfo (StrictnessInfo wrapper_args bot)
  = hsep [ptext SLIT("__S"), pprDemands wrapper_args bot]
\end{code}

%************************************************************************
%*									*
\subsection[worker-IdInfo]{Worker info about an @Id@}
%*									*
%************************************************************************

If this Id has a worker then we store a reference to it. Worker
functions are generated by the worker/wrapper pass.  This uses
information from the strictness and CPR analyses.

There might not be a worker, even for a strict function, because:
(a) the function might be small enough to inline, so no need 
    for w/w split
(b) the strictness info might be "SSS" or something, so no w/w split.

\begin{code}

type WorkerInfo = Maybe Id

mkWorkerInfo :: Id -> WorkerInfo
mkWorkerInfo wk_id = Just wk_id

noWorkerInfo = Nothing

ppWorkerInfo Nothing      = empty
ppWorkerInfo (Just wk_id) = ppr wk_id

workerExists :: Maybe Id -> Bool
workerExists = isJust
\end{code}


%************************************************************************
%*									*
\subsection[update-IdInfo]{Update-analysis info about an @Id@}
%*									*
%************************************************************************

\begin{code}
data UpdateInfo
  = NoUpdateInfo
  | SomeUpdateInfo UpdateSpec
  deriving (Eq, Ord)
      -- we need Eq/Ord to cross-chk update infos in interfaces

-- the form in which we pass update-analysis info between modules:
type UpdateSpec = [Int]
\end{code}

\begin{code}
mkUpdateInfo = SomeUpdateInfo

updateInfoMaybe NoUpdateInfo	    = Nothing
updateInfoMaybe (SomeUpdateInfo []) = Nothing
updateInfoMaybe (SomeUpdateInfo	 u) = Just u
\end{code}

Text instance so that the update annotations can be read in.

\begin{code}
ppUpdateInfo NoUpdateInfo	   = empty
ppUpdateInfo (SomeUpdateInfo [])   = empty
ppUpdateInfo (SomeUpdateInfo spec) = (<>) (ptext SLIT("__U ")) (hcat (map int spec))
\end{code}

%************************************************************************
%*									*
\subsection[CAF-IdInfo]{CAF-related information}
%*									*
%************************************************************************

This information is used to build Static Reference Tables (see
simplStg/ComputeSRT.lhs).

\begin{code}
data CafInfo 
	= MayHaveCafRefs		-- either:
					-- (1) A function or static constructor
					--     that refers to one or more CAFs,
					-- (2) A real live CAF

	| NoCafRefs			-- A function or static constructor
				        -- that refers to no CAFs.

-- LATER: not sure how easy this is...
--      | OneCafRef Id


ppCafInfo NoCafRefs = ptext SLIT("__C")
ppCafInfo MayHaveCafRefs = empty
\end{code}

%************************************************************************
%*									*
\subsection[cpr-IdInfo]{Constructed Product Result info about an @Id@}
%*									*
%************************************************************************

If the @Id@ is a function then it may have CPR info. A CPR analysis
phase detects whether:

\begin{enumerate}
\item
The function's return value has a product type, i.e. an algebraic  type 
with a single constructor. Examples of such types are tuples and boxed
primitive values.
\item
The function always 'constructs' the value that it is returning.  It
must do this on every path through,  and it's OK if it calls another
function which constructs the result.
\end{enumerate}

If this is the case then we store a template which tells us the
function has the CPR property and which components of the result are
also CPRs.   

\begin{code}
data CprInfo
  = NoCPRInfo

  | CPRInfo [CprInfo] 

-- e.g. const 5 == CPRInfo [NoCPRInfo]
--              == __M(-)
--      \x -> (5,
--              (x,
--               5,
--               x)
--            ) 
--            CPRInfo [CPRInfo [NoCPRInfo], 
--                     CPRInfo [NoCprInfo,
--                              CPRInfo [NoCPRInfo],
--                              NoCPRInfo]
--                    ]
--            __M((-)(-(-)-)-)
\end{code}

\begin{code}

noCprInfo       = NoCPRInfo

ppCprInfo NoCPRInfo = empty
ppCprInfo c@(CPRInfo _)
  = hsep [ptext SLIT("__M"), ppCprInfo' c]
    where
    ppCprInfo' NoCPRInfo      = char '-'
    ppCprInfo' (CPRInfo args) = parens (hcat (map ppCprInfo' args))

instance Outputable CprInfo where
    ppr = ppCprInfo

instance Show CprInfo where
    showsPrec p c = showsPrecSDoc p (ppr c)
\end{code}



