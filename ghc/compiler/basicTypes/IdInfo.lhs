%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
module IdInfo (
	GlobalIdDetails(..), notGlobalId, 	-- Not abstract

	IdInfo,		-- Abstract
	vanillaIdInfo, noCafNoTyGenIdInfo,
	seqIdInfo, megaSeqIdInfo,

	-- Zapping
	zapLamInfo, zapDemandInfo,
	shortableIdInfo, copyIdInfo,

	-- Arity
	ArityInfo,
	unknownArity, 
	arityInfo, setArityInfo, ppArityInfo, 

	-- New demand and strictness info
 	newStrictnessInfo, setNewStrictnessInfo, 
  	newDemandInfo, setNewDemandInfo, newDemand, oldDemand,

	-- Strictness; imported from Demand
	StrictnessInfo(..),
	mkStrictnessInfo, noStrictnessInfo,
	ppStrictnessInfo,isBottomingStrictness, 
	strictnessInfo, setStrictnessInfo, setAllStrictnessInfo,
	oldStrictnessFromNew, newStrictnessFromOld, cprInfoFromNewStrictness,

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
	InlinePragInfo, 
	inlinePragInfo, setInlinePragInfo, 

	-- Occurrence info
	OccInfo(..), isFragileOcc, isDeadOcc, isLoopBreaker,
	InsideLam, OneBranch, insideLam, notInsideLam, oneBranch, notOneBranch,
	occInfo, setOccInfo, 

	-- Specialisation
	specInfo, setSpecInfo,

	-- CG info
	CgInfo(..), cgInfo, setCgInfo,  pprCgInfo,
 	cgCafInfo, vanillaCgInfo,
	CgInfoEnv, lookupCgInfo,

	-- CAF info
	CafInfo(..), ppCafInfo, setCafInfo, mayHaveCafRefs,

        -- Constructed Product Result Info
        CprInfo(..), cprInfo, setCprInfo, ppCprInfo, noCprInfo,

        -- Lambda-bound variable info
        LBVarInfo(..), lbvarInfo, setLBVarInfo, noLBVarInfo, hasNoLBVarInfo
    ) where

#include "HsVersions.h"


import CoreSyn
import Type		( Type, usOnce, eqUsage )
import PrimOp	 	( PrimOp )
import NameEnv		( NameEnv, lookupNameEnv )
import Name		( Name )
import Var              ( Id )
import BasicTypes	( OccInfo(..), isFragileOcc, isDeadOcc, seqOccInfo, isLoopBreaker,
			  InsideLam, insideLam, notInsideLam, 
			  OneBranch, oneBranch, notOneBranch,
			  Arity,
			  Activation(..)
			)
import DataCon		( DataCon )
import ForeignCall	( ForeignCall )
import FieldLabel	( FieldLabel )
import Type		( usOnce, usMany )
import Demand		hiding( Demand )
import qualified Demand
import NewDemand	( Demand(..), DmdResult(..), Demands(..),
			  lazyDmd, topDmd, dmdTypeDepth, isStrictDmd, isBotRes, 
			  splitStrictSig, strictSigResInfo,
			  StrictSig, mkStrictSig, mkTopDmdType, evalDmd, lazyDmd
			)
import Outputable	
import Util		( seqList, listLengthCmp )
import List		( replicate )

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
	  `setOccInfo`,
	  `setCgInfo`,
	  `setCafInfo`,
	  `setNewStrictnessInfo`,
	  `setAllStrictnessInfo`,
	  `setNewDemandInfo`
	-- infixl so you can say (id `set` a `set` b)
\end{code}

%************************************************************************
%*									*
\subsection{New strictness info}
%*									*
%************************************************************************

To be removed later

\begin{code}
setAllStrictnessInfo :: IdInfo -> Maybe StrictSig -> IdInfo
-- Set old and new strictness info
setAllStrictnessInfo info Nothing
  = info { newStrictnessInfo = Nothing, 
	   strictnessInfo = NoStrictnessInfo, 
	   cprInfo = NoCPRInfo }
setAllStrictnessInfo info (Just sig)
  = info { newStrictnessInfo = Just sig, 
	   strictnessInfo = oldStrictnessFromNew sig, 
	   cprInfo = cprInfoFromNewStrictness sig }

oldStrictnessFromNew :: StrictSig -> Demand.StrictnessInfo
oldStrictnessFromNew sig = mkStrictnessInfo (map oldDemand dmds, isBotRes res_info)
			 where
			   (dmds, res_info) = splitStrictSig sig

cprInfoFromNewStrictness :: StrictSig -> CprInfo
cprInfoFromNewStrictness sig = case strictSigResInfo sig of
				  RetCPR -> ReturnsCPR
				  other  -> NoCPRInfo

newStrictnessFromOld :: Name -> Arity -> Demand.StrictnessInfo -> CprInfo -> StrictSig
newStrictnessFromOld name arity (Demand.StrictnessInfo ds res) cpr
  | listLengthCmp ds arity /= GT -- length ds <= arity
	-- Sometimes the old strictness analyser has more
	-- demands than the arity justifies
  = mk_strict_sig name arity $
    mkTopDmdType (map newDemand ds) (newRes res cpr)

newStrictnessFromOld name arity other cpr
  =	-- Either no strictness info, or arity is too small
	-- In either case we can't say anything useful
    mk_strict_sig name arity $
    mkTopDmdType (replicate arity lazyDmd) (newRes False cpr)

mk_strict_sig name arity dmd_ty
  = WARN( arity /= dmdTypeDepth dmd_ty, ppr name <+> (ppr arity $$ ppr dmd_ty) )
    mkStrictSig dmd_ty

newRes True  _ 	        = BotRes
newRes False ReturnsCPR = RetCPR
newRes False NoCPRInfo  = TopRes

newDemand :: Demand.Demand -> NewDemand.Demand
newDemand (WwLazy True)      = Abs
newDemand (WwLazy False)     = lazyDmd
newDemand WwStrict	     = evalDmd
newDemand (WwUnpack unpk ds) = Eval (Prod (map newDemand ds))
newDemand WwPrim	     = lazyDmd
newDemand WwEnum	     = evalDmd

oldDemand :: NewDemand.Demand -> Demand.Demand
oldDemand Abs	     	   = WwLazy True
oldDemand Top	     	   = WwLazy False
oldDemand Bot	     	   = WwStrict
oldDemand (Box Bot)	   = WwStrict
oldDemand (Box Abs)	   = WwLazy False
oldDemand (Box (Eval _))   = WwStrict	-- Pass box only
oldDemand (Defer d)        = WwLazy False
oldDemand (Eval (Prod ds)) = WwUnpack True (map oldDemand ds)
oldDemand (Eval (Poly _))  = WwStrict
oldDemand (Call _)         = WwStrict
\end{code}


%************************************************************************
%*									*
\subsection{GlobalIdDetails
%*									*
%************************************************************************

This type is here (rather than in Id.lhs) mainly because there's 
an IdInfo.hi-boot, but no Id.hi-boot, and GlobalIdDetails is imported
(recursively) by Var.lhs.

\begin{code}
data GlobalIdDetails
  = VanillaGlobal		-- Imported from elsewhere, a default method Id.

  | RecordSelId FieldLabel	-- The Id for a record selector
  | DataConId DataCon		-- The Id for a data constructor *worker*
  | DataConWrapId DataCon	-- The Id for a data constructor *wrapper*
				-- [the only reasons we need to know is so that
				--  a) we can  suppress printing a definition in the interface file
				--  b) when typechecking a pattern we can get from the
				--     Id back to the data con]

  | PrimOpId PrimOp		-- The Id for a primitive operator
  | FCallId ForeignCall		-- The Id for a foreign call

  | NotGlobalId			-- Used as a convenient extra return value from globalIdDetails
    
notGlobalId = NotGlobalId

instance Outputable GlobalIdDetails where
    ppr NotGlobalId       = ptext SLIT("[***NotGlobalId***]")
    ppr VanillaGlobal     = ptext SLIT("[GlobalId]")
    ppr (DataConId _)     = ptext SLIT("[DataCon]")
    ppr (DataConWrapId _) = ptext SLIT("[DataConWrapper]")
    ppr (PrimOpId _)      = ptext SLIT("[PrimOp]")
    ppr (FCallId _)       = ptext SLIT("[ForeignCall]")
    ppr (RecordSelId _)   = ptext SLIT("[RecSel]")
\end{code}


%************************************************************************
%*									*
\subsection{The main IdInfo type}
%*									*
%************************************************************************

An @IdInfo@ gives {\em optional} information about an @Id@.  If
present it never lies, but it may not be present, in which case there
is always a conservative assumption which can be made.

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
	arityInfo 	:: ArityInfo,		-- Its arity
	demandInfo 	:: Demand.Demand,	-- Whether or not it is definitely demanded
	specInfo 	:: CoreRules,		-- Specialisations of this function which exist
        tyGenInfo       :: TyGenInfo,           -- Restrictions on usage-generalisation of this Id
	strictnessInfo	:: StrictnessInfo,	-- Strictness properties
        workerInfo      :: WorkerInfo,          -- Pointer to Worker Function
	unfoldingInfo	:: Unfolding,		-- Its unfolding
	cgInfo		:: CgInfo,		-- Code generator info (arity, CAF info)
	cprInfo 	:: CprInfo,             -- Function always constructs a product result
        lbvarInfo	:: LBVarInfo,		-- Info about a lambda-bound variable
	inlinePragInfo	:: InlinePragInfo,	-- Inline pragma
	occInfo		:: OccInfo,		-- How it occurs

	newStrictnessInfo :: Maybe StrictSig,	-- Reason for Maybe: the DmdAnal phase needs to
						-- know whether whether this is the first visit,
						-- so it can assign botSig.  Other customers want
						-- topSig.  So Nothing is good.
	newDemandInfo	  :: Demand
    }

seqIdInfo :: IdInfo -> ()
seqIdInfo (IdInfo {}) = ()

megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqArity (arityInfo info)			`seq`
    seqDemand (demandInfo info)			`seq`
    seqRules (specInfo info)			`seq`
    seqTyGenInfo (tyGenInfo info)               `seq`
    seqStrictnessInfo (strictnessInfo info)	`seq`
    seqWorker (workerInfo info)			`seq`

--    seqUnfolding (unfoldingInfo info)	`seq`
-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all

-- CgInfo is involved in a loop, so we have to be careful not to seq it
-- too early.
--    seqCg (cgInfo info)			`seq`
    seqCpr (cprInfo info)		`seq`
    seqLBVar (lbvarInfo info)		`seq`
    seqOccInfo (occInfo info) 
\end{code}

Setters

\begin{code}
setWorkerInfo     info wk = wk `seq` info { workerInfo = wk }
setSpecInfo 	  info sp = sp `seq` info { specInfo = sp }
setTyGenInfo      info tg = tg `seq` info { tyGenInfo = tg }
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo	  info oc = oc `seq` info { occInfo = oc }
setStrictnessInfo info st = st `seq` info { strictnessInfo = st }
	-- Try to avoid spack leaks by seq'ing

setUnfoldingInfo  info uf 
  | isEvaldUnfolding uf
	-- If the unfolding is a value, the demand info may
	-- go pear-shaped, so we nuke it.  Example:
	--	let x = (a,b) in
	--	case x of (p,q) -> h p q x
	-- Here x is certainly demanded. But after we've nuked
	-- the case, we'll get just
	--	let x = (a,b) in h a b x
	-- and now x is not demanded (I'm assuming h is lazy)
	-- This really happens.  The solution here is a bit ad hoc...
  = info { unfoldingInfo = uf, newDemandInfo = Top }

  | otherwise
	-- We do *not* seq on the unfolding info, For some reason, doing so 
	-- actually increases residency significantly. 
  = info { unfoldingInfo = uf }

setDemandInfo	  info dd = info { demandInfo = dd }
setArityInfo	  info ar = info { arityInfo = ar  }
setCgInfo         info cg = info { cgInfo = cg }
setCprInfo        info cp = info { cprInfo = cp }
setLBVarInfo      info lb = info { lbvarInfo = lb }

setNewDemandInfo     info dd = info { newDemandInfo = dd }
setNewStrictnessInfo info dd = info { newStrictnessInfo = dd }
\end{code}


\begin{code}
vanillaIdInfo :: IdInfo
vanillaIdInfo 
  = IdInfo {
	    cgInfo		= noCgInfo,
	    arityInfo		= unknownArity,
	    demandInfo		= wwLazy,
	    specInfo		= emptyCoreRules,
            tyGenInfo		= noTyGenInfo,
	    workerInfo		= NoWorker,
	    strictnessInfo	= NoStrictnessInfo,
	    unfoldingInfo	= noUnfolding,
	    cprInfo		= NoCPRInfo,
	    lbvarInfo		= NoLBVarInfo,
	    inlinePragInfo 	= AlwaysActive,
	    occInfo		= NoOccInfo,
	    newDemandInfo	= topDmd,
	    newStrictnessInfo   = Nothing
	   }

noCafNoTyGenIdInfo = vanillaIdInfo `setTyGenInfo` TyGenNever
			      	   `setCgInfo`    CgInfo NoCafRefs
	-- Used for built-in type Ids in MkId.
	-- Many built-in things have fixed types, so we shouldn't
	-- run around generalising them
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
type ArityInfo = Arity
  	-- A partial application of this Id to up to n-1 value arguments
	-- does essentially no work.  That is not necessarily the
	-- same as saying that it has n leading lambdas, because coerces
	-- may get in the way.

	-- The arity might increase later in the compilation process, if
	-- an extra lambda floats up to the binding site.

seqArity :: ArityInfo -> ()
seqArity a = a `seq` ()

unknownArity = 0 :: Arity

ppArityInfo 0 = empty
ppArityInfo n = hsep [ptext SLIT("Arity"), int n]
\end{code}

%************************************************************************
%*									*
\subsection{Inline-pragma information}
%*									*
%************************************************************************

\begin{code}
type InlinePragInfo = Activation
	-- Tells when the inlining is active
	-- When it is active the thing may be inlined, depending on how
	-- big it is.
	--
	-- If there was an INLINE pragma, then as a separate matter, the
	-- RHS will have been made to look small with a CoreSyn Inline Note
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
  where go  Nothing              	 = 'x'  -- for legibility, choose
        go (Just u) | u `eqUsage` usOnce = '1'  -- chars with identity
                    | u `eqUsage` usMany = 'M'  -- Z-encoding.
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

Sometimes the arity of a wrapper changes from the original arity from
which it was generated, so we always emit the "original" arity into
the interface file, as part of the worker info.

How can this happen?  Sometimes we get
	f = coerce t (\x y -> $wf x y)
at the moment of w/w split; but the eta reducer turns it into
	f = coerce t $wf
which is perfectly fine except that the exposed arity so far as
the code generator is concerned (zero) differs from the arity
when we did the split (2).  

All this arises because we use 'arity' to mean "exactly how many
top level lambdas are there" in interface files; but during the
compilation of this module it means "how many things can I apply
this to".

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
\subsection[CG-IdInfo]{Code generator-related information}
%*									*
%************************************************************************

CgInfo encapsulates calling-convention information produced by the code 
generator.  It is pasted into the IdInfo of each emitted Id by CoreTidy,
but only as a thunk --- the information is only actually produced further
downstream, by the code generator.

\begin{code}
#ifndef DEBUG
newtype CgInfo = CgInfo CafInfo	-- We are back to only having CafRefs in CgInfo
noCgInfo = panic "NoCgInfo!"
#else
data CgInfo = CgInfo CafInfo
	    | NoCgInfo		-- In debug mode we don't want a black hole here
				-- See Id.idCgInfo
	-- noCgInfo is used for local Ids, which shouldn't need any CgInfo
noCgInfo = NoCgInfo
#endif

cgCafInfo (CgInfo caf_info) = caf_info

setCafInfo info caf_info = info `setCgInfo` CgInfo caf_info 

seqCg c = c `seq` ()  -- fields are strict anyhow

vanillaCgInfo = CgInfo MayHaveCafRefs		-- Definitely safe

-- CafInfo is used to build Static Reference Tables (see simplStg/SRT.lhs).

data CafInfo 
	= MayHaveCafRefs		-- either:
					-- (1) A function or static constructor
					--     that refers to one or more CAFs,
					-- (2) A real live CAF

	| NoCafRefs			-- A function or static constructor
				        -- that refers to no CAFs.

mayHaveCafRefs  MayHaveCafRefs = True
mayHaveCafRefs _	       = False

seqCaf c = c `seq` ()

pprCgInfo (CgInfo caf_info) = ppCafInfo caf_info

ppArity 0 = empty
ppArity n = hsep [ptext SLIT("__A"), int n]

ppCafInfo NoCafRefs = ptext SLIT("__C")
ppCafInfo MayHaveCafRefs = empty
\end{code}

\begin{code}
type CgInfoEnv = NameEnv CgInfo

lookupCgInfo :: NameEnv CgInfo -> Name -> CgInfo
lookupCgInfo env n = case lookupNameEnv env n of
			Just info -> info
			Nothing   -> pprTrace "Urk! Not in CgInfo env" (ppr n) vanillaCgInfo
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
hasNoLBVarInfo NoLBVarInfo = True
hasNoLBVarInfo other       = False

noLBVarInfo = NoLBVarInfo

-- not safe to print or parse LBVarInfo because it is not really a
-- property of the definition, but a property of the context.
pprLBVarInfo NoLBVarInfo     = empty
pprLBVarInfo (LBVarInfo u)   | u `eqUsage` usOnce
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

@zapLamInfo@ is used for lambda binders that turn out to to be
part of an unsaturated lambda

\begin{code}
zapLamInfo :: IdInfo -> Maybe IdInfo
zapLamInfo info@(IdInfo {occInfo = occ, newDemandInfo = demand})
  | is_safe_occ && not (isStrictDmd demand)
  = Nothing
  | otherwise
  = Just (info {occInfo = safe_occ,
		newDemandInfo = Top})
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

\begin{code}
zapDemandInfo :: IdInfo -> Maybe IdInfo
zapDemandInfo info@(IdInfo {newDemandInfo = demand})
  | not (isStrictDmd demand) = Nothing
  | otherwise		     = Just (info {newDemandInfo = Top})
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
	iterate f x -> build (\cn -> iterateFB c f x)
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
