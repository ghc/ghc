%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
module IdInfo (
	IdInfo,		-- Abstract

	vanillaIdInfo, constantIdInfo, mkIdInfo, seqIdInfo, megaSeqIdInfo,

	-- Zapping
	zapFragileInfo,	zapLamInfo, zapSpecPragInfo, shortableIdInfo, copyIdInfo,

	-- Flavour
	IdFlavour(..), flavourInfo, 
	setNoDiscardInfo, setFlavourInfo,
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

        -- Usage generalisation
        TyGenInfo(..),
        tyGenInfo, setTyGenInfo,
        noTyGenInfo, isNoTyGenInfo, ppTyGenInfo, tyGenInfoString,

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
	isNeverInlinePrag, neverInlinePrag,

	-- Occurrence info
	OccInfo(..), isFragileOcc, isDeadOcc, isLoopBreaker,
	InsideLam, OneBranch, insideLam, notInsideLam, oneBranch, notOneBranch,
	occInfo, setOccInfo, 

	-- Specialisation
	specInfo, setSpecInfo,

	-- CAF info
	CafInfo(..), cafInfo, setCafInfo, ppCafInfo,

        -- Constructed Product Result Info
        CprInfo(..), cprInfo, setCprInfo, ppCprInfo, noCprInfo,

        -- Lambda-bound variable info
        LBVarInfo(..), lbvarInfo, setLBVarInfo, noLBVarInfo
    ) where

#include "HsVersions.h"


import CoreSyn
import Type		( Type, usOnce )
import PrimOp	 	( PrimOp )
import Var              ( Id )
import BasicTypes	( OccInfo(..), isFragileOcc, isDeadOcc, seqOccInfo, isLoopBreaker,
			  InsideLam, insideLam, notInsideLam, 
			  OneBranch, oneBranch, notOneBranch,
			  Arity
			)
import DataCon		( DataCon )
import FieldLabel	( FieldLabel )
import Type		( usOnce, usMany )
import Demand		-- Lots of stuff
import Outputable	
import Util		( seqList )

infixl 	1 `setDemandInfo`,
    	  `setTyGenInfo`,
	  `setStrictnessInfo`,
	  `setSpecInfo`,
	  `setArityInfo`,
	  `setInlinePragInfo`,
	  `setUnfoldingInfo`,
	  `setCprInfo`,
	  `setWorkerInfo`,
	  `setLBVarInfo`,
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
        tyGenInfo       :: TyGenInfo,           -- Restrictions on usage-generalisation of this Id
	strictnessInfo	:: StrictnessInfo,	-- Strictness properties
        workerInfo      :: WorkerInfo,          -- Pointer to Worker Function
	unfoldingInfo	:: Unfolding,		-- Its unfolding
	cafInfo		:: CafInfo,		-- whether it refers (indirectly) to any CAFs
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
    seqTyGenInfo (tyGenInfo info)               `seq`
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
setFlavourInfo    info fl = fl `seq` info { flavourInfo = fl }
setWorkerInfo     info wk = wk `seq` info { workerInfo = wk }
setSpecInfo 	  info sp = PSEQ sp (info { specInfo = sp })
setTyGenInfo      info tg = tg `seq` info { tyGenInfo = tg }
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

setDemandInfo	  info dd = info { demandInfo = dd }
setArityInfo	  info ar = info { arityInfo = ar  }
setCafInfo        info cf = info { cafInfo = cf }
setCprInfo        info cp = info { cprInfo = cp }
setLBVarInfo      info lb = info { lbvarInfo = lb }

setNoDiscardInfo  info = case flavourInfo info of
				VanillaId -> info { flavourInfo = ExportedId }
				other	  -> info
zapSpecPragInfo   info = case flavourInfo info of
				SpecPragmaId -> info { flavourInfo = VanillaId }
				other	     -> info
\end{code}


\begin{code}
vanillaIdInfo :: IdInfo
	-- Used for locally-defined Ids
	-- We are going to calculate correct CAF information at the end
vanillaIdInfo = mkIdInfo VanillaId NoCafRefs

constantIdInfo :: IdInfo
	-- Used for imported Ids
	-- The default is that they *do* have CAFs; an interface-file pragma
	-- may say "oh no it doesn't", but in the absence of such a pragma
	-- we'd better assume it does
constantIdInfo = mkIdInfo ConstantId MayHaveCafRefs

mkIdInfo :: IdFlavour -> CafInfo -> IdInfo
mkIdInfo flv caf 
  = IdInfo {
	    flavourInfo		= flv,
	    cafInfo		= caf,
	    arityInfo		= UnknownArity,
	    demandInfo		= wwLazy,
	    specInfo		= emptyCoreRules,
            tyGenInfo		= noTyGenInfo,
	    workerInfo		= NoWorker,
	    strictnessInfo	= NoStrictnessInfo,
	    unfoldingInfo	= noUnfolding,
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
  = VanillaId			-- Locally defined, not exported
  | ExportedId			-- Locally defined, exported
  | SpecPragmaId		-- Locally defined, RHS holds specialised call

  | ConstantId 			-- Imported from elsewhere, or a default method Id.

  | DictFunId			-- We flag dictionary functions so that we can
				-- conveniently extract the DictFuns from a set of
				-- bindings when building a module's interface

  | DataConId DataCon		-- The Id for a data constructor *worker*
  | DataConWrapId DataCon	-- The Id for a data constructor *wrapper*
				-- [the only reasons we need to know is so that
				--  a) we can  suppress printing a definition in the interface file
				--  b) when typechecking a pattern we can get from the
				--     Id back to the data con]
  | PrimOpId PrimOp		-- The Id for a primitive operator
  | RecordSelId FieldLabel	-- The Id for a record selector


ppFlavourInfo :: IdFlavour -> SDoc
ppFlavourInfo VanillaId         = empty
ppFlavourInfo ExportedId        = ptext SLIT("[Exported]")
ppFlavourInfo SpecPragmaId    	= ptext SLIT("[SpecPrag]")
ppFlavourInfo ConstantId        = ptext SLIT("[Constant]")
ppFlavourInfo DictFunId         = ptext SLIT("[DictFun]")
ppFlavourInfo (DataConId _)     = ptext SLIT("[DataCon]")
ppFlavourInfo (DataConWrapId _) = ptext SLIT("[DataConWrapper]")
ppFlavourInfo (PrimOpId _)    	= ptext SLIT("[PrimOp]")
ppFlavourInfo (RecordSelId _) 	= ptext SLIT("[RecSel]")

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

  | ArityAtLeast Arity	-- A partial application of this Id to up to n-1 value arguments
			-- does essentially no work.  That is not necessarily the
			-- same as saying that it has n leading lambdas, because coerces
			-- may get in the way.

			-- functions in the module being compiled.  Their arity
			-- might increase later in the compilation process, if
			-- an extra lambda floats up to the binding site.
  deriving( Eq )

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
  deriving( Eq )
	-- The True, Nothing case doesn't need to be recorded

	-- SEE COMMENTS WITH CoreUnfold.blackListed on the
	-- exact significance of the IMustNotBeINLINEd pragma

isNeverInlinePrag :: InlinePragInfo -> Bool
isNeverInlinePrag (IMustNotBeINLINEd _ Nothing) = True
isNeverInlinePrag other			        = False

neverInlinePrag :: InlinePragInfo
neverInlinePrag = IMustNotBeINLINEd True{-should be False? --SDM -} Nothing

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
%*                                                                    *
\subsection[TyGen-IdInfo]{Type generalisation info about an @Id@}
%*                                                                    *
%************************************************************************

Certain passes (notably usage inference) may change the type of an
identifier, modifying all in-scope uses of that identifier
appropriately to maintain type safety.

However, some identifiers must not have their types changed in this
way, because their types are conjured up in the front end of the
compiler rather than being read from the interface file.  Default
methods, dictionary functions, record selectors, and others are in
this category.  (see comment at TcClassDcl.tcClassSig).

To indicate this property, such identifiers are marked TyGenNever.

Furthermore, if the usage inference generates a usage-specialised
variant of a function, we must NOT re-infer a fully-generalised type
at the next inference.  This finer property is indicated by a
TyGenUInfo on the identifier.

\begin{code}
data TyGenInfo
  = NoTyGenInfo              -- no restriction on type generalisation

  | TyGenUInfo [Maybe Type]  -- restrict generalisation of this Id to
                             -- preserve specified usage annotations

  | TyGenNever               -- never generalise the type of this Id

  deriving ( Eq )
\end{code}

For TyGenUInfo, the list has one entry for each usage annotation on
the type of the Id, in left-to-right pre-order (annotations come
before the type they annotate).  Nothing means no restriction; Just
usOnce or Just usMany forces that annotation to that value.  Other
usage annotations are illegal.

\begin{code}
seqTyGenInfo :: TyGenInfo -> ()
seqTyGenInfo  NoTyGenInfo    = ()
seqTyGenInfo (TyGenUInfo us) = seqList us ()
seqTyGenInfo  TyGenNever     = ()

noTyGenInfo :: TyGenInfo
noTyGenInfo = NoTyGenInfo

isNoTyGenInfo :: TyGenInfo -> Bool
isNoTyGenInfo NoTyGenInfo = True
isNoTyGenInfo _           = False

-- NB: There's probably no need to write this information out to the interface file.
-- Why?  Simply because imported identifiers never get their types re-inferred.
-- But it's definitely nice to see in dumps, it for debugging purposes.

ppTyGenInfo :: TyGenInfo -> SDoc
ppTyGenInfo  NoTyGenInfo    = empty
ppTyGenInfo (TyGenUInfo us) = ptext SLIT("__G") <+> text (tyGenInfoString us)
ppTyGenInfo  TyGenNever     = ptext SLIT("__G N")

tyGenInfoString us = map go us
  where go  Nothing               = 'x'  -- for legibility, choose
        go (Just u) | u == usOnce = '1'  -- chars with identity
                    | u == usMany = 'M'  -- Z-encoding.
        go other = pprPanic "IdInfo.tyGenInfoString: unexpected annotation" (ppr other)

instance Outputable TyGenInfo where
  ppr = ppTyGenInfo

instance Show TyGenInfo where
  showsPrec p c = showsPrecSDoc p (ppr c)
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
	-- w/w split. See comments in MkIface.ifaceId, with the 'Worker' code.

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

  | LBVarInfo Type		-- The lambda that binds this Id has this usage
				--   annotation (i.e., if ==usOnce, then the
				--   lambda is applied at most once).
				-- The annotation's kind must be `$'
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
pprLBVarInfo (LBVarInfo u)   | u == usOnce
                             = getPprStyle $ \ sty ->
                               if ifaceStyle sty
                               then empty
                               else ptext SLIT("OneShot")
                             | otherwise
                             = empty

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
  |  not (isFragileOcc occ)
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

BUT (a) we must be careful about messing up rules
    (b) we must ensure f's IdInfo ends up right

(a) Messing up the rules
~~~~~~~~~~~~~~~~~~~~
The example that went bad on me was this one:
	
    iterate :: (a -> a) -> a -> [a]
    iterate = iterateList
    
    iterateFB c f x = x `c` iterateFB c f (f x)
    iterateList f x =  x : iterateList f (f x)
    
    {-# RULES
    "iterate"	forall f x.	iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB" 		iterateFB (:) = iterateList
     #-}

This got shorted out to:

    iterateList :: (a -> a) -> a -> [a]
    iterateList = iterate
    
    iterateFB c f x = x `c` iterateFB c f (f x)
    iterate f x =  x : iterate f (f x)
    
    {-# RULES
    "iterate"	forall f x.	iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB" 		iterateFB (:) = iterate
     #-}

And now we get an infinite loop in the rule system 
	iterate f x -> build (\cn -> iterateFB c f x
		    -> iterateFB (:) f x
		    -> iterate f x

Tiresome solution: don't do shorting out if f has rewrite rules.
Hence shortableIdInfo.

(b) Keeping the IdInfo right
~~~~~~~~~~~~~~~~~~~~~~~~
We want to move strictness/worker info from f_local to f, but keep the rest.
Hence copyIdInfo.

\begin{code}
shortableIdInfo :: IdInfo -> Bool
shortableIdInfo info = isEmptyCoreRules (specInfo info)

copyIdInfo :: IdInfo	-- f_local
  	   -> IdInfo	-- f (the exported one)
	   -> IdInfo	-- New info for f
copyIdInfo f_local f = f { strictnessInfo = strictnessInfo f_local,
			   workerInfo     = workerInfo     f_local,
			   cprInfo        = cprInfo        f_local
			  }
\end{code}
