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

	-- Zapping
	zapFragileInfo,	zapLamInfo, zapSpecPragInfo, copyIdInfo,

	-- Flavour
	IdFlavour(..), flavourInfo, 
	setNoDiscardInfo,
	ppFlavourInfo,

	-- Arity
	ArityInfo(..),
	exactArity, atLeastArity, unknownArity, hasArity,
	arityInfo, setArityInfo, ppArityInfo, arityLowerBound,

	-- Strictness; imported from Demand
	StrictnessInfo(..),
	mkStrictnessInfo, noStrictnessInfo,
	ppStrictnessInfo,isBottomingStrictness, 

	strictnessInfo, setStrictnessInfo, 	

        -- Worker
        WorkerInfo(..), workerExists, wrapperArity, workerId,
        workerInfo, setWorkerInfo, ppWorkerInfo,

	-- Unfolding
	unfoldingInfo, setUnfoldingInfo, 

	-- DemandInfo
	demandInfo, setDemandInfo, 

	-- Inline prags
	InlinePragInfo(..), 
	inlinePragInfo, setInlinePragInfo, pprInlinePragInfo,

	-- Occurrence info
	OccInfo(..), isFragileOccInfo,
	InsideLam, OneBranch, insideLam, notInsideLam, oneBranch, notOneBranch,
	occInfo, setOccInfo, 

	-- Specialisation
	specInfo, setSpecInfo,

	-- Update
	UpdateInfo, UpdateSpec,
	mkUpdateInfo, updateInfo, updateInfoMaybe, ppUpdateInfo, setUpdateInfo,

	-- CAF info
	CafInfo(..), cafInfo, setCafInfo, ppCafInfo,

        -- Constructed Product Result Info
        CprInfo(..), cprInfo, setCprInfo, ppCprInfo, noCprInfo,

        -- Lambda-bound variable info
        LBVarInfo(..), lbvarInfo, setLBVarInfo, noLBVarInfo
    ) where

#include "HsVersions.h"


import CoreSyn
import PrimOp	 	( PrimOp )
import Var              ( Id )
import BasicTypes	( OccInfo(..), isFragileOccInfo, seqOccInfo,
			  InsideLam, insideLam, notInsideLam, 
			  OneBranch, oneBranch, notOneBranch,
			  Arity
			)
import DataCon		( DataCon )
import FieldLabel	( FieldLabel )
import Demand		-- Lots of stuff
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
	  `setCafInfo`,
	  `setOccInfo`
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
	inlinePragInfo	:: InlinePragInfo,	-- Inline pragma
	occInfo		:: OccInfo		-- How it occurs
    }

seqIdInfo :: IdInfo -> ()
seqIdInfo (IdInfo {}) = ()

megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqFlavour (flavourInfo info) 		`seq`
    seqArity (arityInfo info)			`seq`
    seqDemand (demandInfo info)			`seq`
    seqRules (specInfo info)			`seq`
    seqStrictnessInfo (strictnessInfo info)	`seq`
    seqWorker (workerInfo info)			`seq`

--    seqUnfolding (unfoldingInfo info)	`seq`
-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all

    seqCaf (cafInfo info)		`seq`
    seqCpr (cprInfo info)		`seq`
    seqLBVar (lbvarInfo info)		`seq`
    seqOccInfo (occInfo info) 
\end{code}

Setters

\begin{code}
setWorkerInfo     info wk = wk `seq` info { workerInfo = wk }
setSpecInfo 	  info sp = PSEQ sp (info { specInfo = sp })
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo	  info oc = oc `seq` info { occInfo = oc }
setStrictnessInfo info st = st `seq` info { strictnessInfo = st }
	-- Try to avoid spack leaks by seq'ing

setUnfoldingInfo  info uf 
  | isEvaldUnfolding uf && isStrict (demandInfo info)
	-- If the unfolding is a value, the demand info may
	-- go pear-shaped, so we nuke it.  Example:
	--	let x = (a,b) in
	--	case x of (p,q) -> h p q x
	-- Here x is certainly demanded. But after we've nuked
	-- the case, we'll get just
	--	let x = (a,b) in h a b x
	-- and now x is not demanded (I'm assuming h is lazy)
	-- This really happens.  The solution here is a bit ad hoc...
  = info { unfoldingInfo = uf, demandInfo = wwLazy }

  | otherwise
	-- We do *not* seq on the unfolding info, For some reason, doing so 
	-- actually increases residency significantly. 
  = info { unfoldingInfo = uf }

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
		    workerInfo		= NoWorker,
		    strictnessInfo	= NoStrictnessInfo,
		    unfoldingInfo	= noUnfolding,
		    updateInfo		= NoUpdateInfo,
		    cafInfo		= MayHaveCafRefs,
		    cprInfo		= NoCPRInfo,
		    lbvarInfo		= NoLBVarInfo,
		    inlinePragInfo 	= NoInlinePragInfo,
		    occInfo		= NoOccInfo
	   }
\end{code}


%************************************************************************
%*									*
\subsection{Flavour}
%*									*
%************************************************************************

\begin{code}
data IdFlavour
  = VanillaId 			-- Most Ids are like this
  | DataConId DataCon		-- The Id for a data constructor *worker*
  | DataConWrapId DataCon	-- The Id for a data constructor *wrapper*
				-- [the only reasons we need to know is so that
				--  a) we can  suppress printing a definition in the interface file
				--  b) when typechecking a pattern we can get from the
				--     Id back to the data con]
  | PrimOpId PrimOp		-- The Id for a primitive operator
  | RecordSelId FieldLabel	-- The Id for a record selector
  | SpecPragmaId		-- Don't discard these
  | NoDiscardId			-- Don't discard these either

ppFlavourInfo :: IdFlavour -> SDoc
ppFlavourInfo VanillaId         = empty
ppFlavourInfo (DataConId _)     = ptext SLIT("[DataCon]")
ppFlavourInfo (DataConWrapId _) = ptext SLIT("[DataConWrapper]")
ppFlavourInfo (PrimOpId _)    	= ptext SLIT("[PrimOp]")
ppFlavourInfo (RecordSelId _) 	= ptext SLIT("[RecSel]")
ppFlavourInfo SpecPragmaId    	= ptext SLIT("[SpecPrag]")
ppFlavourInfo NoDiscardId     	= ptext SLIT("[NoDiscard]")

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

  | ArityExactly Arity	-- Arity is exactly this.  We use this when importing a
			-- function; it's already been compiled and we know its
			-- arity for sure.

  | ArityAtLeast Arity	-- Arity is this or greater.  We attach this arity to 
			-- functions in the module being compiled.  Their arity
			-- might increase later in the compilation process, if
			-- an extra lambda floats up to the binding site.

seqArity :: ArityInfo -> ()
seqArity a = arityLowerBound a `seq` ()

exactArity   = ArityExactly
atLeastArity = ArityAtLeast
unknownArity = UnknownArity

arityLowerBound :: ArityInfo -> Arity
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
  | IMustNotBeINLINEd Bool		-- True <=> came from an INLINE prag, False <=> came from a NOINLINE prag
		      (Maybe Int)	-- Phase number from pragma, if any
	-- The True, Nothing case doesn't need to be recorded

instance Outputable InlinePragInfo where
  -- This is now parsed in interface files
  ppr NoInlinePragInfo = empty
  ppr other_prag       = ptext SLIT("__U") <> pprInlinePragInfo other_prag

pprInlinePragInfo NoInlinePragInfo  		     = empty
pprInlinePragInfo (IMustNotBeINLINEd True Nothing)   = empty
pprInlinePragInfo (IMustNotBeINLINEd True (Just n))  = brackets (int n)
pprInlinePragInfo (IMustNotBeINLINEd False Nothing)  = brackets (char '!')
pprInlinePragInfo (IMustNotBeINLINEd False (Just n)) = brackets (char '!' <> int n)
							
instance Show InlinePragInfo where
  showsPrec p prag = showsPrecSDoc p (ppr prag)
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

data WorkerInfo = NoWorker
		| HasWorker Id Arity
	-- The Arity is the arity of the *wrapper* at the moment of the
	-- w/w split. It had better be the same as the arity of the wrapper
	-- at the moment it is spat into the interface file.
	-- This Arity just lets us make a (hopefully redundant) sanity check

seqWorker :: WorkerInfo -> ()
seqWorker (HasWorker id _) = id `seq` ()
seqWorker NoWorker	   = ()

ppWorkerInfo NoWorker            = empty
ppWorkerInfo (HasWorker wk_id _) = ptext SLIT("__P") <+> ppr wk_id

noWorkerInfo = NoWorker

workerExists :: WorkerInfo -> Bool
workerExists NoWorker        = False
workerExists (HasWorker _ _) = True

workerId :: WorkerInfo -> Id
workerId (HasWorker id _) = id

wrapperArity :: WorkerInfo -> Arity
wrapperArity (HasWorker _ a) = a
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
  | ReturnsCPR	-- Yes, this function returns a constructed product
		-- Implicitly, this means "after the function has been applied
		-- to all its arguments", so the worker/wrapper builder in 
		-- WwLib.mkWWcpr checks that that it is indeed saturated before
		-- making use of the CPR info

	-- We used to keep nested info about sub-components, but
	-- we never used it so I threw it away
\end{code}

\begin{code}
seqCpr :: CprInfo -> ()
seqCpr ReturnsCPR = ()
seqCpr NoCPRInfo  = ()

noCprInfo       = NoCPRInfo

ppCprInfo NoCPRInfo  = empty
ppCprInfo ReturnsCPR = ptext SLIT("__M")

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


%************************************************************************
%*									*
\subsection{Bulk operations on IdInfo}
%*									*
%************************************************************************

zapFragileInfo is used when cloning binders, mainly in the
simplifier.  We must forget about used-once information because that
isn't necessarily correct in the transformed program.
Also forget specialisations and unfoldings because they would need
substitution to be correct.  (They get pinned back on separately.)

\begin{code}
zapFragileInfo :: IdInfo -> Maybe IdInfo
zapFragileInfo info@(IdInfo {occInfo		= occ, 
			     workerInfo		= wrkr,
			     specInfo		= rules, 
			     unfoldingInfo	= unfolding})
  |  not (isFragileOccInfo occ)
        -- We must forget about whether it was marked safe-to-inline,
	-- because that isn't necessarily true in the simplified expression.
	-- This is important because expressions may  be re-simplified
	-- We don't zap deadness or loop-breaker-ness.
	-- The latter is important because it tells MkIface not to 
	-- spit out an inlining for the thing.  The former doesn't
	-- seem so important, but there's no harm.

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
  = Just (info {occInfo		= robust_occ_info,
		workerInfo	= noWorkerInfo,
		specInfo	= emptyCoreRules,
		unfoldingInfo	= noUnfolding})
  where
	-- It's important to keep the loop-breaker info,
	-- because the substitution doesn't remember it.
    robust_occ_info = case occ of
			OneOcc _ _ -> NoOccInfo
			other	   -> occ
\end{code}

@zapLamInfo@ is used for lambda binders that turn out to to be
part of an unsaturated lambda

\begin{code}
zapLamInfo :: IdInfo -> Maybe IdInfo
zapLamInfo info@(IdInfo {occInfo = occ, demandInfo = demand})
  | is_safe_occ && not (isStrict demand)
  = Nothing
  | otherwise
  = Just (info {occInfo = safe_occ,
		demandInfo = wwLazy})
  where
	-- The "unsafe" occ info is the ones that say I'm not in a lambda
	-- because that might not be true for an unsaturated lambda
    is_safe_occ = case occ of
		 	OneOcc in_lam once -> in_lam
			other	  	   -> True

    safe_occ = case occ of
		 OneOcc _ once -> OneOcc insideLam once
		 other	       -> occ
\end{code}


copyIdInfo is used when shorting out a top-level binding
	f_local = BIG
	f = f_local
where f is exported.  We are going to swizzle it around to
	f = BIG
	f_local = f
but we must be careful to combine their IdInfos right.
The fact that things can go wrong here is a bad sign, but I can't see
how to make it 'patently right', so copyIdInfo is derived (pretty much) by trial and error

Here 'from' is f_local, 'to' is f, and the result is attached to f

\begin{code}
copyIdInfo :: IdInfo	-- From
  	   -> IdInfo	-- To
	   -> IdInfo	-- To, updated with stuff from From; except flavour unchanged
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
