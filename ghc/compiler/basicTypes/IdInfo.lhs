%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
module IdInfo (
	IdInfo,		-- Abstract

	vanillaIdInfo, mkIdInfo, seqIdInfo, megaSeqIdInfo,

	-- Flavour
	IdFlavour(..), flavourInfo, 
	setNoDiscardInfo, zapSpecPragInfo, copyIdInfo,
	ppFlavourInfo,

	-- Arity
	ArityInfo(..),
	exactArity, atLeastArity, unknownArity, hasArity,
	arityInfo, setArityInfo, ppArityInfo, arityLowerBound,

	-- Strictness
	StrictnessInfo(..),				-- Non-abstract
	mkStrictnessInfo,
	noStrictnessInfo, strictnessInfo,
	ppStrictnessInfo, setStrictnessInfo, 
	isBottomingStrictness, appIsBottom,

        -- Worker
        WorkerInfo, workerExists, 
        workerInfo, setWorkerInfo, ppWorkerInfo,

	-- Unfolding
	unfoldingInfo, setUnfoldingInfo, 

	-- DemandInfo
	demandInfo, setDemandInfo, 

	-- Inline prags
	InlinePragInfo(..), OccInfo(..),
	inlinePragInfo, setInlinePragInfo, notInsideLambda,

	-- Specialisation
	specInfo, setSpecInfo,

	-- Update
	UpdateInfo, UpdateSpec,
	mkUpdateInfo, updateInfo, updateInfoMaybe, ppUpdateInfo, setUpdateInfo,

	-- CAF info
	CafInfo(..), cafInfo, setCafInfo, ppCafInfo,

        -- Constructed Product Result Info
        CprInfo(..), cprInfo, setCprInfo, ppCprInfo, noCprInfo,

	-- Zapping
 	zapLamIdInfo, zapFragileIdInfo, zapIdInfoForStg,

        -- Lambda-bound variable info
        LBVarInfo(..), lbvarInfo, setLBVarInfo, noLBVarInfo
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} CoreUnfold ( Unfolding, noUnfolding, hasUnfolding, seqUnfolding )
import {-# SOURCE #-} CoreSyn	 ( CoreExpr, CoreRules, emptyCoreRules, isEmptyCoreRules, seqRules )
import {-# SOURCE #-} Const	 ( Con )

import Var              ( Id )
import FieldLabel	( FieldLabel )
import Demand		( Demand, isStrict, isLazy, wwLazy, pprDemands, seqDemand, seqDemands )
import Type             ( UsageAnn )
import Outputable	
import Maybe            ( isJust )

infixl 	1 `setUpdateInfo`,
	  `setDemandInfo`,
	  `setStrictnessInfo`,
	  `setSpecInfo`,
	  `setArityInfo`,
	  `setInlinePragInfo`,
	  `setUnfoldingInfo`,
	  `setCprInfo`,
	  `setWorkerInfo`,
	  `setCafInfo`
	-- infixl so you can say (id `set` a `set` b)
\end{code}

An @IdInfo@ gives {\em optional} information about an @Id@.  If
present it never lies, but it may not be present, in which case there
is always a conservative assumption which can be made.

	There is one exception: the 'flavour' is *not* optional.
	You must not discard it.
	It used to be in Var.lhs, but that seems unclean.

Two @Id@s may have different info even though they have the same
@Unique@ (and are hence the same @Id@); for example, one might lack
the properties attached to the other.

The @IdInfo@ gives information about the value, or definition, of the
@Id@.  It does {\em not} contain information about the @Id@'s usage
(except for @DemandInfo@? ToDo). (@lbvarInfo@ is also a marginal
case.  KSW 1999-04).

\begin{code}
data IdInfo
  = IdInfo {
	flavourInfo	:: IdFlavour,		-- NOT OPTIONAL
	arityInfo 	:: ArityInfo,		-- Its arity
	demandInfo 	:: Demand,		-- Whether or not it is definitely demanded
	specInfo 	:: CoreRules,		-- Specialisations of this function which exist
	strictnessInfo	:: StrictnessInfo,	-- Strictness properties
        workerInfo      :: WorkerInfo,          -- Pointer to Worker Function
	unfoldingInfo	:: Unfolding,		-- Its unfolding
	updateInfo	:: UpdateInfo,		-- Which args should be updated
	cafInfo		:: CafInfo,
	cprInfo 	:: CprInfo,             -- Function always constructs a product result
        lbvarInfo	:: LBVarInfo,		-- Info about a lambda-bound variable
	inlinePragInfo	:: InlinePragInfo	-- Inline pragmas
    }

seqIdInfo :: IdInfo -> ()
seqIdInfo (IdInfo {}) = ()

megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqFlavour (flavourInfo info) 	`seq`
    seqArity (arityInfo info)		`seq`
    seqDemand (demandInfo info)		`seq`
    seqRules (specInfo info)		`seq`
    seqStrictness (strictnessInfo info)	`seq`
    seqWorker (workerInfo info)		`seq`

--    seqUnfolding (unfoldingInfo info)	`seq`
-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all

    seqCaf (cafInfo info)		`seq`
    seqCpr (cprInfo info)		`seq`
    seqLBVar (lbvarInfo info)		`seq`
    seqInlinePrag (inlinePragInfo info) 
\end{code}

Setters

\begin{code}
setWorkerInfo     info wk = wk `seq` info { workerInfo = wk }
setSpecInfo 	  info sp = PSEQ sp (info { specInfo = sp })
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setStrictnessInfo info st = st `seq` info { strictnessInfo = st }
	-- Try to avoid spack leaks by seq'ing

setUnfoldingInfo  info uf = info { unfoldingInfo = uf }
	-- We do *not* seq on the unfolding info, For some reason, doing so 
	-- actually increases residency significantly. 

setUpdateInfo	  info ud = info { updateInfo = ud }
setDemandInfo	  info dd = info { demandInfo = dd }
setArityInfo	  info ar = info { arityInfo = ar  }
setCafInfo        info cf = info { cafInfo = cf }
setCprInfo        info cp = info { cprInfo = cp }
setLBVarInfo      info lb = info { lbvarInfo = lb }

setNoDiscardInfo  info = case flavourInfo info of
				VanillaId -> info { flavourInfo = NoDiscardId }
				other	  -> info
zapSpecPragInfo   info = case flavourInfo info of
				SpecPragmaId -> info { flavourInfo = VanillaId }
				other	     -> info

copyIdInfo :: IdInfo	-- From
  	   -> IdInfo	-- To
	   -> IdInfo	-- To, updated with stuff from From; except flavour unchanged
-- copyIdInfo is used when shorting out a top-level binding
--	f_local = BIG
--	f = f_local
-- where f is exported.  We are going to swizzle it around to
--	f = BIG
--	f_local = f
-- but we must be careful to combine their IdInfos right.
-- The fact that things can go wrong here is a bad sign, but I can't see
-- how to make it 'patently right', so copyIdInfo is derived (pretty much) by trial and error
--
-- Here 'from' is f_local, 'to' is f, and the result is attached to f

copyIdInfo from to = from { flavourInfo = flavourInfo to,
			    specInfo = specInfo to,
			    inlinePragInfo = inlinePragInfo to
			  }
	-- It's important to preserve the inline pragma on 'f'; e.g. consider
	--	{-# NOINLINE f #-}
	--	f = local
	--
	-- similarly, transformation rules may be attached to f
	-- and we want to preserve them.  
	--
	-- On the other hand, we want the strictness info from f_local.
\end{code}


\begin{code}
vanillaIdInfo :: IdInfo
vanillaIdInfo = mkIdInfo VanillaId

mkIdInfo :: IdFlavour -> IdInfo
mkIdInfo flv = IdInfo {
		    flavourInfo		= flv,
		    arityInfo		= UnknownArity,
		    demandInfo		= wwLazy,
		    specInfo		= emptyCoreRules,
		    workerInfo		= Nothing,
		    strictnessInfo	= NoStrictnessInfo,
		    unfoldingInfo	= noUnfolding,
		    updateInfo		= NoUpdateInfo,
		    cafInfo		= MayHaveCafRefs,
		    cprInfo		= NoCPRInfo,
		    lbvarInfo		= NoLBVarInfo,
		    inlinePragInfo 	= NoInlinePragInfo
	   }
\end{code}


%************************************************************************
%*									*
\subsection{Flavour}
%*									*
%************************************************************************

\begin{code}
data IdFlavour
  = VanillaId 				-- Most Ids are like this
  | ConstantId Con			-- The Id for a constant (data constructor or primop)
  | RecordSelId FieldLabel		-- The Id for a record selector
  | SpecPragmaId			-- Don't discard these
  | NoDiscardId				-- Don't discard these either

ppFlavourInfo :: IdFlavour -> SDoc
ppFlavourInfo VanillaId       = empty
ppFlavourInfo (ConstantId _)  = ptext SLIT("[Constr]")
ppFlavourInfo (RecordSelId _) = ptext SLIT("[RecSel]")
ppFlavourInfo SpecPragmaId    = ptext SLIT("[SpecPrag]")
ppFlavourInfo NoDiscardId     = ptext SLIT("[NoDiscard]")

seqFlavour :: IdFlavour -> ()
seqFlavour f = f `seq` ()
\end{code}

The @SpecPragmaId@ exists only to make Ids that are
on the *LHS* of bindings created by SPECIALISE pragmas; 
eg:		s = f Int d
The SpecPragmaId is never itself mentioned; it
exists solely so that the specialiser will find
the call to f, and make specialised version of it.
The SpecPragmaId binding is discarded by the specialiser
when it gathers up overloaded calls.
Meanwhile, it is not discarded as dead code.


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

  | ArityExactly Int	-- Arity is exactly this.  We use this when importing a
			-- function; it's already been compiled and we know its
			-- arity for sure.

  | ArityAtLeast Int	-- Arity is this or greater.  We attach this arity to 
			-- functions in the module being compiled.  Their arity
			-- might increase later in the compilation process, if
			-- an extra lambda floats up to the binding site.

seqArity :: ArityInfo -> ()
seqArity a = arityLowerBound a `seq` ()

exactArity   = ArityExactly
atLeastArity = ArityAtLeast
unknownArity = UnknownArity

arityLowerBound :: ArityInfo -> Int
arityLowerBound UnknownArity     = 0
arityLowerBound (ArityAtLeast n) = n
arityLowerBound (ArityExactly n) = n

hasArity :: ArityInfo -> Bool
hasArity UnknownArity = False
hasArity other	      = True

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

seqInlinePrag :: InlinePragInfo -> ()
seqInlinePrag (ICanSafelyBeINLINEd occ alts) 
  = occ `seq` alts `seq` ()
seqInlinePrag other
  = ()

instance Outputable InlinePragInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoInlinePragInfo  	= empty
  ppr IMustBeINLINEd    	= ptext SLIT("__UU")
  ppr IMustNotBeINLINEd 	= ptext SLIT("__Unot")
  ppr IAmALoopBreaker   	= ptext SLIT("__Ux")
  ppr IAmDead			= ptext SLIT("__Ud")
  ppr (ICanSafelyBeINLINEd InsideLam _) = ptext SLIT("__Ul")
  ppr (ICanSafelyBeINLINEd NotInsideLam True)  = ptext SLIT("__Us")
  ppr (ICanSafelyBeINLINEd NotInsideLam False) = ptext SLIT("__Us*")

instance Show InlinePragInfo where
  showsPrec p prag = showsPrecSDoc p (ppr prag)
\end{code}

\begin{code}
data OccInfo
  = NotInsideLam

  | InsideLam		-- Inside a non-linear lambda (that is, a lambda which
			-- is sure to be instantiated only once).
			-- Substituting a redex for this occurrence is
			-- dangerous because it might duplicate work.

instance Outputable OccInfo where
  ppr NotInsideLam = empty
  ppr InsideLam    = text "l"


notInsideLambda :: OccInfo -> Bool
notInsideLambda NotInsideLam = True
notInsideLambda InsideLam    = False
\end{code}

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

seqStrictness :: StrictnessInfo -> ()
seqStrictness (StrictnessInfo ds b) = b `seq` seqDemands ds
seqStrictness other		    = ()
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

{- UNUSED:
mkWorkerInfo :: Id -> WorkerInfo
mkWorkerInfo wk_id = Just wk_id
-}

seqWorker :: WorkerInfo -> ()
seqWorker (Just id) = id `seq` ()
seqWorker Nothing   = ()

ppWorkerInfo Nothing      = empty
ppWorkerInfo (Just wk_id) = ptext SLIT("__P") <+> ppr wk_id

noWorkerInfo = Nothing

workerExists :: WorkerInfo -> Bool
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
ppUpdateInfo (SomeUpdateInfo spec) = (<>) (ptext SLIT("__UA ")) (hcat (map int spec))
  -- was "__U "; changed to avoid conflict with unfoldings.  KSW 1999-07.
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


seqCaf c = c `seq` ()

ppCafInfo NoCafRefs = ptext SLIT("__C")
ppCafInfo MayHaveCafRefs = empty
\end{code}


%************************************************************************
%*									*
\subsection[CAF-IdInfo]{CAF-related information}
%*									*
%************************************************************************

zapFragileIdInfo is used when cloning binders, mainly in the
simplifier.  We must forget about used-once information because that
isn't necessarily correct in the transformed program.
Also forget specialisations and unfoldings because they would need
substitution to be correct.  (They get pinned back on separately.)

\begin{code}
zapFragileIdInfo :: IdInfo -> Maybe IdInfo
zapFragileIdInfo info@(IdInfo {inlinePragInfo	= inline_prag, 
			       workerInfo	= wrkr,
			       specInfo		= rules, 
			       unfoldingInfo	= unfolding})
  |  not is_fragile_inline_prag 
        -- We must forget about whether it was marked safe-to-inline,
	-- because that isn't necessarily true in the simplified expression.
	-- This is important because expressions may  be re-simplified

  && isEmptyCoreRules rules
	-- Specialisations would need substituting.  They get pinned
	-- back on separately.

  && not (workerExists wrkr)

  && not (hasUnfolding unfolding)
	-- This is very important; occasionally a let-bound binder is used
	-- as a binder in some lambda, in which case its unfolding is utterly
	-- bogus.  Also the unfolding uses old binders so if we left it we'd
	-- have to substitute it. Much better simply to give the Id a new
	-- unfolding each time, which is what the simplifier does.
  = Nothing

  | otherwise
  = Just (info {inlinePragInfo	= safe_inline_prag, 
		workerInfo	= noWorkerInfo,
		specInfo	= emptyCoreRules,
		unfoldingInfo	= noUnfolding})

  where
    is_fragile_inline_prag = case inline_prag of
				ICanSafelyBeINLINEd _ _ -> True

-- We used to say the dead-ness was fragile, but I don't
-- see why it is.  Furthermore, deadness is a pain to lose;
-- see Simplify.mkDupableCont (Select ...)
--				IAmDead			-> True

				other			-> False

	-- Be careful not to destroy real 'pragma' info
    safe_inline_prag | is_fragile_inline_prag = NoInlinePragInfo
		     | otherwise	      = inline_prag
\end{code}


@zapLamIdInfo@ is used for lambda binders that turn out to to be
part of an unsaturated lambda

\begin{code}
zapLamIdInfo :: IdInfo -> Maybe IdInfo
zapLamIdInfo info@(IdInfo {inlinePragInfo = inline_prag, demandInfo = demand})
  | is_safe_inline_prag && not (isStrict demand)
  = Nothing
  | otherwise
  = Just (info {inlinePragInfo = safe_inline_prag,
		demandInfo = wwLazy})
  where
	-- The "unsafe" prags are the ones that say I'm not in a lambda
	-- because that might not be true for an unsaturated lambda
    is_safe_inline_prag = case inline_prag of
			 	ICanSafelyBeINLINEd NotInsideLam nalts -> False
				other				       -> True

    safe_inline_prag    = case inline_prag of
			 	ICanSafelyBeINLINEd _ nalts
				      -> ICanSafelyBeINLINEd InsideLam nalts
				other -> inline_prag
\end{code}

\begin{code}
zapIdInfoForStg :: IdInfo -> IdInfo
	-- Return only the info needed for STG stuff
	-- Namely, nothing, I think
zapIdInfoForStg info = vanillaIdInfo	
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
seqCpr :: CprInfo -> ()
seqCpr (CPRInfo cs) = seqCprs cs
seqCpr NoCPRInfo    = ()

seqCprs [] = ()
seqCprs (c:cs) = seqCpr c `seq` seqCprs cs


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


%************************************************************************
%*									*
\subsection[lbvar-IdInfo]{Lambda-bound var info about an @Id@}
%*									*
%************************************************************************

If the @Id@ is a lambda-bound variable then it may have lambda-bound
var info.  The usage analysis (UsageSP) detects whether the lambda
binding this var is a ``one-shot'' lambda; that is, whether it is
applied at most once.

This information may be useful in optimisation, as computations may
safely be floated inside such a lambda without risk of duplicating
work.

\begin{code}
data LBVarInfo
  = NoLBVarInfo

  | IsOneShotLambda		-- The lambda that binds this Id is applied
				--   at most once
				-- HACK ALERT! placing this info here is a short-term hack,
				--   but it minimises changes to the rest of the compiler.
				--   Hack agreed by SLPJ/KSW 1999-04.

seqLBVar l = l `seq` ()
\end{code}

\begin{code}
noLBVarInfo = NoLBVarInfo

-- not safe to print or parse LBVarInfo because it is not really a
-- property of the definition, but a property of the context.
pprLBVarInfo NoLBVarInfo     = empty
pprLBVarInfo IsOneShotLambda = getPprStyle $ \ sty ->
                               if ifaceStyle sty then empty
                                                 else ptext SLIT("OneShot")

instance Outputable LBVarInfo where
    ppr = pprLBVarInfo

instance Show LBVarInfo where
    showsPrec p c = showsPrecSDoc p (ppr c)
\end{code}
