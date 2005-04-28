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
	vanillaIdInfo, noCafIdInfo,
	seqIdInfo, megaSeqIdInfo,

	-- Zapping
	zapLamInfo, zapDemandInfo,

	-- Arity
	ArityInfo,
	unknownArity, 
	arityInfo, setArityInfo, ppArityInfo, 

	-- New demand and strictness info
 	newStrictnessInfo, setNewStrictnessInfo, 
  	newDemandInfo, setNewDemandInfo, pprNewStrictness,
	setAllStrictnessInfo,

#ifdef OLD_STRICTNESS
	-- Strictness; imported from Demand
	StrictnessInfo(..),
	mkStrictnessInfo, noStrictnessInfo,
	ppStrictnessInfo,isBottomingStrictness, 
#endif

        -- Worker
        WorkerInfo(..), workerExists, wrapperArity, workerId,
        workerInfo, setWorkerInfo, ppWorkerInfo,

	-- Unfolding
	unfoldingInfo, setUnfoldingInfo, setUnfoldingInfoLazily,

#ifdef OLD_STRICTNESS
	-- Old DemandInfo and StrictnessInfo
	demandInfo, setDemandInfo, 
	strictnessInfo, setStrictnessInfo,
        cprInfoFromNewStrictness,
	oldStrictnessFromNew, newStrictnessFromOld,
	oldDemand, newDemand,

        -- Constructed Product Result Info
        CprInfo(..), cprInfo, setCprInfo, ppCprInfo, noCprInfo,
#endif

	-- Inline prags
	InlinePragInfo, 
	inlinePragInfo, setInlinePragInfo, 

	-- Occurrence info
	OccInfo(..), isFragileOcc, isDeadOcc, isLoopBreaker,
	InsideLam, OneBranch, insideLam, notInsideLam, oneBranch, notOneBranch,
	occInfo, setOccInfo, 

	-- Specialisation
	SpecInfo(..), specInfo, setSpecInfo, isEmptySpecInfo, 
	specInfoFreeVars, specInfoRules, seqSpecInfo,

	-- CAF info
	CafInfo(..), cafInfo, ppCafInfo, setCafInfo, mayHaveCafRefs,

        -- Lambda-bound variable info
        LBVarInfo(..), lbvarInfo, setLBVarInfo, noLBVarInfo, hasNoLBVarInfo
    ) where

#include "HsVersions.h"


import CoreSyn
import Class		( Class )
import PrimOp	 	( PrimOp )
import Var              ( Id )
import VarSet		( VarSet, emptyVarSet, seqVarSet )
import BasicTypes	( OccInfo(..), isFragileOcc, isDeadOcc, seqOccInfo, isLoopBreaker,
			  InsideLam, insideLam, notInsideLam, 
			  OneBranch, oneBranch, notOneBranch,
			  Arity,
			  Activation(..)
			)
import DataCon		( DataCon )
import TyCon		( TyCon, FieldLabel )
import ForeignCall	( ForeignCall )
import NewDemand
import Outputable	
import Maybe		( isJust )

#ifdef OLD_STRICTNESS
import Name		( Name )
import Demand		hiding( Demand, seqDemand )
import qualified Demand
import Util		( listLengthCmp )
import List		( replicate )
#endif

-- infixl so you can say (id `set` a `set` b)
infixl 	1 `setSpecInfo`,
	  `setArityInfo`,
	  `setInlinePragInfo`,
	  `setUnfoldingInfo`,
	  `setWorkerInfo`,
	  `setLBVarInfo`,
	  `setOccInfo`,
	  `setCafInfo`,
	  `setNewStrictnessInfo`,
	  `setAllStrictnessInfo`,
	  `setNewDemandInfo`
#ifdef OLD_STRICTNESS
	  , `setCprInfo`
	  , `setDemandInfo`
	  , `setStrictnessInfo`
#endif
\end{code}

%************************************************************************
%*									*
\subsection{New strictness info}
%*									*
%************************************************************************

To be removed later

\begin{code}
-- setAllStrictnessInfo :: IdInfo -> Maybe StrictSig -> IdInfo
-- Set old and new strictness info
setAllStrictnessInfo info Nothing
  = info { newStrictnessInfo = Nothing
#ifdef OLD_STRICTNESS
         , strictnessInfo = NoStrictnessInfo
         , cprInfo = NoCPRInfo
#endif
         }

setAllStrictnessInfo info (Just sig)
  = info { newStrictnessInfo = Just sig
#ifdef OLD_STRICTNESS
         , strictnessInfo = oldStrictnessFromNew sig
         , cprInfo = cprInfoFromNewStrictness sig
#endif
         }

seqNewStrictnessInfo Nothing = ()
seqNewStrictnessInfo (Just ty) = seqStrictSig ty

pprNewStrictness Nothing = empty
pprNewStrictness (Just sig) = ftext FSLIT("Str:") <+> ppr sig

#ifdef OLD_STRICTNESS
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
newRes False ReturnsCPR = retCPR
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

#endif /* OLD_STRICTNESS */
\end{code}


\begin{code}
seqNewDemandInfo Nothing    = ()
seqNewDemandInfo (Just dmd) = seqDemand dmd
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

  | RecordSelId TyCon FieldLabel  -- The Id for a record selector

  | DataConWorkId DataCon	-- The Id for a data constructor *worker*
  | DataConWrapId DataCon	-- The Id for a data constructor *wrapper*
				-- [the only reasons we need to know is so that
				--  a) we can  suppress printing a definition in the interface file
				--  b) when typechecking a pattern we can get from the
				--     Id back to the data con]

  | ClassOpId Class		-- An operation of a class

  | PrimOpId PrimOp		-- The Id for a primitive operator
  | FCallId ForeignCall		-- The Id for a foreign call

  | NotGlobalId			-- Used as a convenient extra return value from globalIdDetails
    
notGlobalId = NotGlobalId

instance Outputable GlobalIdDetails where
    ppr NotGlobalId       = ptext SLIT("[***NotGlobalId***]")
    ppr VanillaGlobal     = ptext SLIT("[GlobalId]")
    ppr (DataConWorkId _) = ptext SLIT("[DataCon]")
    ppr (DataConWrapId _) = ptext SLIT("[DataConWrapper]")
    ppr (ClassOpId _)     = ptext SLIT("[ClassOp]")
    ppr (PrimOpId _)      = ptext SLIT("[PrimOp]")
    ppr (FCallId _)       = ptext SLIT("[ForeignCall]")
    ppr (RecordSelId _ _) = ptext SLIT("[RecSel]")
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
	arityInfo 	:: !ArityInfo,		-- Its arity
	specInfo 	:: SpecInfo,		-- Specialisations of this function which exist
#ifdef OLD_STRICTNESS
	cprInfo 	:: CprInfo,             -- Function always constructs a product result
	demandInfo 	:: Demand.Demand,	-- Whether or not it is definitely demanded
	strictnessInfo	:: StrictnessInfo,	-- Strictness properties
#endif
        workerInfo      :: WorkerInfo,          -- Pointer to Worker Function
						-- Within one module this is irrelevant; the 
						-- inlining of a worker is handled via the Unfolding
						-- WorkerInfo is used *only* to indicate the form of
						-- the RHS, so that interface files don't actually 
						-- need to contain the RHS; it can be derived from
						-- the strictness info

	unfoldingInfo	:: Unfolding,		-- Its unfolding
	cafInfo		:: CafInfo,		-- CAF info
        lbvarInfo	:: LBVarInfo,		-- Info about a lambda-bound variable
	inlinePragInfo	:: InlinePragInfo,	-- Inline pragma
	occInfo		:: OccInfo,		-- How it occurs

	newStrictnessInfo :: Maybe StrictSig,	-- Reason for Maybe: the DmdAnal phase needs to
						-- know whether whether this is the first visit,
						-- so it can assign botSig.  Other customers want
						-- topSig.  So Nothing is good.

	newDemandInfo	  :: Maybe Demand	-- Similarly we want to know if there's no
						-- known demand yet, for when we are looking for
						-- CPR info
    }

seqIdInfo :: IdInfo -> ()
seqIdInfo (IdInfo {}) = ()

megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqSpecInfo (specInfo info)			`seq`
    seqWorker (workerInfo info)			`seq`

-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all
--    seqUnfolding (unfoldingInfo info)		`seq`

    seqNewDemandInfo (newDemandInfo info)	`seq`
    seqNewStrictnessInfo (newStrictnessInfo info) `seq`

#ifdef OLD_STRICTNESS
    Demand.seqDemand (demandInfo info)		`seq`
    seqStrictnessInfo (strictnessInfo info)	`seq`
    seqCpr (cprInfo info)			`seq`
#endif

    seqCaf (cafInfo info)			`seq`
    seqLBVar (lbvarInfo info)			`seq`
    seqOccInfo (occInfo info) 
\end{code}

Setters

\begin{code}
setWorkerInfo     info wk = wk `seq` info { workerInfo = wk }
setSpecInfo 	  info sp = sp `seq` info { specInfo = sp }
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo	  info oc = oc `seq` info { occInfo = oc }
#ifdef OLD_STRICTNESS
setStrictnessInfo info st = st `seq` info { strictnessInfo = st }
#endif
	-- Try to avoid spack leaks by seq'ing

setUnfoldingInfoLazily info uf 	-- Lazy variant to avoid looking at the
  =				-- unfolding of an imported Id unless necessary
    info { unfoldingInfo = uf }	-- (In this case the demand-zapping is redundant.)

setUnfoldingInfo info uf 
	-- We do *not* seq on the unfolding info, For some reason, doing so 
	-- actually increases residency significantly. 
  = info { unfoldingInfo = uf }

#ifdef OLD_STRICTNESS
setDemandInfo	  info dd = info { demandInfo = dd }
setCprInfo        info cp = info { cprInfo = cp }
#endif

setArityInfo	  info ar  = info { arityInfo = ar  }
setCafInfo        info caf = info { cafInfo = caf }

setLBVarInfo      info lb = {-lb `seq`-} info { lbvarInfo = lb }

setNewDemandInfo     info dd = dd `seq` info { newDemandInfo = dd }
setNewStrictnessInfo info dd = dd `seq` info { newStrictnessInfo = dd }
\end{code}


\begin{code}
vanillaIdInfo :: IdInfo
vanillaIdInfo 
  = IdInfo {
	    cafInfo		= vanillaCafInfo,
	    arityInfo		= unknownArity,
#ifdef OLD_STRICTNESS
	    cprInfo		= NoCPRInfo,
	    demandInfo		= wwLazy,
	    strictnessInfo	= NoStrictnessInfo,
#endif
	    specInfo		= emptySpecInfo,
	    workerInfo		= NoWorker,
	    unfoldingInfo	= noUnfolding,
	    lbvarInfo		= NoLBVarInfo,
	    inlinePragInfo 	= AlwaysActive,
	    occInfo		= NoOccInfo,
	    newDemandInfo	= Nothing,
	    newStrictnessInfo   = Nothing
	   }

noCafIdInfo  = vanillaIdInfo `setCafInfo`    NoCafRefs
	-- Used for built-in type Ids in MkId.
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
%*									*
	SpecInfo
%*									*
%************************************************************************

\begin{code}
-- CoreRules is used only in an idSpecialisation (move to IdInfo?)
data SpecInfo 
  = SpecInfo [CoreRule] VarSet	-- Locally-defined free vars of RHSs

emptySpecInfo :: SpecInfo
emptySpecInfo = SpecInfo [] emptyVarSet

isEmptySpecInfo :: SpecInfo -> Bool
isEmptySpecInfo (SpecInfo rs _) = null rs

specInfoFreeVars :: SpecInfo -> VarSet
specInfoFreeVars (SpecInfo _ fvs) = fvs

specInfoRules :: SpecInfo -> [CoreRule]
specInfoRules (SpecInfo rules _) = rules

seqSpecInfo (SpecInfo rules fvs) = seqRules rules `seq` seqVarSet fvs
\end{code}


%************************************************************************
%*									*
\subsection[worker-IdInfo]{Worker info about an @Id@}
%*									*
%************************************************************************

If this Id has a worker then we store a reference to it. Worker
functions are generated by the worker/wrapper pass.  This uses
information from strictness analysis.

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
	-- w/w split.  See notes above.

seqWorker :: WorkerInfo -> ()
seqWorker (HasWorker id a) = id `seq` a `seq` ()
seqWorker NoWorker	   = ()

ppWorkerInfo NoWorker            = empty
ppWorkerInfo (HasWorker wk_id _) = ptext SLIT("Worker") <+> ppr wk_id

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

\begin{code}
-- CafInfo is used to build Static Reference Tables (see simplStg/SRT.lhs).

data CafInfo 
	= MayHaveCafRefs		-- either:
					-- (1) A function or static constructor
					--     that refers to one or more CAFs,
					-- (2) A real live CAF

	| NoCafRefs			-- A function or static constructor
				        -- that refers to no CAFs.

vanillaCafInfo = MayHaveCafRefs		-- Definitely safe

mayHaveCafRefs  MayHaveCafRefs = True
mayHaveCafRefs _	       = False

seqCaf c = c `seq` ()

ppCafInfo NoCafRefs = ptext SLIT("NoCafRefs")
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
#ifdef OLD_STRICTNESS
data CprInfo
  = NoCPRInfo
  | ReturnsCPR	-- Yes, this function returns a constructed product
		-- Implicitly, this means "after the function has been applied
		-- to all its arguments", so the worker/wrapper builder in 
		-- WwLib.mkWWcpr checks that that it is indeed saturated before
		-- making use of the CPR info

	-- We used to keep nested info about sub-components, but
	-- we never used it so I threw it away

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
#endif
\end{code}


%************************************************************************
%*									*
\subsection[lbvar-IdInfo]{Lambda-bound var info about an @Id@}
%*									*
%************************************************************************

If the @Id@ is a lambda-bound variable then it may have lambda-bound
var info.  Sometimes we know whether the lambda binding this var is a
``one-shot'' lambda; that is, whether it is applied at most once.

This information may be useful in optimisation, as computations may
safely be floated inside such a lambda without risk of duplicating
work.

\begin{code}
data LBVarInfo = NoLBVarInfo 
	       | IsOneShotLambda	-- The lambda is applied at most once).

seqLBVar l = l `seq` ()
\end{code}

\begin{code}
hasNoLBVarInfo NoLBVarInfo     = True
hasNoLBVarInfo IsOneShotLambda = False

noLBVarInfo = NoLBVarInfo

pprLBVarInfo NoLBVarInfo     = empty
pprLBVarInfo IsOneShotLambda = ptext SLIT("OneShot")

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
  | is_safe_occ occ && is_safe_dmd demand
  = Nothing
  | otherwise
  = Just (info {occInfo = safe_occ, newDemandInfo = Nothing})
  where
	-- The "unsafe" occ info is the ones that say I'm not in a lambda
	-- because that might not be true for an unsaturated lambda
    is_safe_occ (OneOcc in_lam once) = in_lam
    is_safe_occ other		     = True

    safe_occ = case occ of
		 OneOcc _ once -> OneOcc insideLam once
		 other	       -> occ

    is_safe_dmd Nothing    = True
    is_safe_dmd (Just dmd) = not (isStrictDmd dmd)
\end{code}

\begin{code}
zapDemandInfo :: IdInfo -> Maybe IdInfo
zapDemandInfo info@(IdInfo {newDemandInfo = dmd})
  | isJust dmd = Just (info {newDemandInfo = Nothing})
  | otherwise  = Nothing
\end{code}

