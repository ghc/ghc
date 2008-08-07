%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
module IdInfo (
        -- * The GlobalIdDetails type
	GlobalIdDetails(..), notGlobalId, 	-- Not abstract

        -- * The IdInfo type
	IdInfo,		-- Abstract
	vanillaIdInfo, noCafIdInfo,
	seqIdInfo, megaSeqIdInfo,

	-- ** Zapping various forms of Info
	zapLamInfo, zapDemandInfo, zapFragileInfo,

	-- ** The ArityInfo type
	ArityInfo,
	unknownArity, 
	arityInfo, setArityInfo, ppArityInfo, 

	-- ** Demand and strictness Info
 	newStrictnessInfo, setNewStrictnessInfo, 
  	newDemandInfo, setNewDemandInfo, pprNewStrictness,
	setAllStrictnessInfo,

#ifdef OLD_STRICTNESS
	-- ** Old strictness Info
	StrictnessInfo(..),
	mkStrictnessInfo, noStrictnessInfo,
	ppStrictnessInfo, isBottomingStrictness, 
	strictnessInfo, setStrictnessInfo,
	
        oldStrictnessFromNew, newStrictnessFromOld,

	-- ** Old demand Info
	demandInfo, setDemandInfo, 
	oldDemand, newDemand,

        -- ** Old Constructed Product Result Info
        CprInfo(..), 
        cprInfo, setCprInfo, ppCprInfo, noCprInfo,
        cprInfoFromNewStrictness,
#endif

        -- ** The WorkerInfo type
        WorkerInfo(..),
        workerExists, wrapperArity, workerId,
        workerInfo, setWorkerInfo, ppWorkerInfo,

	-- ** Unfolding Info
	unfoldingInfo, setUnfoldingInfo, setUnfoldingInfoLazily,

	-- ** The InlinePragInfo type
	InlinePragInfo,
	inlinePragInfo, setInlinePragInfo,

	-- ** The OccInfo type
	OccInfo(..),
	isFragileOcc, isDeadOcc, isLoopBreaker,
	occInfo, setOccInfo,

	InsideLam, OneBranch,
	insideLam, notInsideLam, oneBranch, notOneBranch,
	
	-- ** The SpecInfo type
	SpecInfo(..),
	isEmptySpecInfo, specInfoFreeVars,
	specInfoRules, seqSpecInfo, setSpecInfoHead,
        specInfo, setSpecInfo,

	-- ** The CAFInfo type
	CafInfo(..),
	ppCafInfo, mayHaveCafRefs,
	cafInfo, setCafInfo,

        -- ** The LBVarInfo type
        LBVarInfo(..),
        noLBVarInfo, hasNoLBVarInfo,
        lbvarInfo, setLBVarInfo,

        -- ** Tick-box Info
        TickBoxOp(..), TickBoxId,
    ) where

import CoreSyn ( CoreRule, setRuleIdName, seqRules, Unfolding, noUnfolding )

import Class
import PrimOp
import Name
import Var
import VarSet
import BasicTypes
import DataCon
import TyCon
import ForeignCall
import NewDemand
import Outputable	
import Module
import FastString

import Data.Maybe

#ifdef OLD_STRICTNESS
import Demand
import qualified Demand
import Util
import Data.List
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
-- | Set old and new strictness information together
setAllStrictnessInfo :: IdInfo -> Maybe StrictSig -> IdInfo
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

seqNewStrictnessInfo :: Maybe StrictSig -> ()
seqNewStrictnessInfo Nothing = ()
seqNewStrictnessInfo (Just ty) = seqStrictSig ty

pprNewStrictness :: Maybe StrictSig -> SDoc
pprNewStrictness Nothing = empty
pprNewStrictness (Just sig) = ftext (fsLit "Str:") <+> ppr sig

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
seqNewDemandInfo :: Maybe Demand -> ()
seqNewDemandInfo Nothing    = ()
seqNewDemandInfo (Just dmd) = seqDemand dmd
\end{code}


%************************************************************************
%*									*
\subsection{GlobalIdDetails}
%*									*
%************************************************************************

This type is here (rather than in Id.lhs) mainly because there's 
an IdInfo.hi-boot, but no Id.hi-boot, and GlobalIdDetails is imported
(recursively) by Var.lhs.

\begin{code}
-- | Information pertaining to global 'Id's. See "Var#globalvslocal" for the distinction 
-- between global and local in this context
data GlobalIdDetails
  = VanillaGlobal		-- ^ The 'Id' is imported from elsewhere or is a default method 'Id'

  -- | The 'Id' for a record selector
  | RecordSelId                 
    { sel_tycon   :: TyCon	-- ^ For a data type family, this is the /instance/ 'TyCon'
				--   not the family 'TyCon'
    , sel_label   :: FieldLabel
    , sel_naughty :: Bool       -- True <=> a "naughty" selector which can't actually exist, for example @x@ in:
                                -- 
                                -- > data T = forall a. MkT { x :: a }
    }				
				-- See Note [Naughty record selectors]
				-- with MkId.mkRecordSelectorId

  | DataConWorkId DataCon	-- ^ The 'Id' is for a data constructor /worker/
  | DataConWrapId DataCon	-- ^ The 'Id' is for a data constructor /wrapper/
				
				-- [the only reasons we need to know is so that
				--  a) to support isImplicitId
				--  b) when desugaring a RecordCon we can get 
				--     from the Id back to the data con]

  | ClassOpId Class		-- ^ The 'Id' is an operation of a class

  | PrimOpId PrimOp		-- ^ The 'Id' is for a primitive operator
  | FCallId ForeignCall		-- ^ The 'Id' is for a foreign call

  | TickBoxOpId TickBoxOp	-- ^ The 'Id' is for a HPC tick box (both traditional and binary)

  | NotGlobalId			-- ^ Used as a convenient extra return value from 'globalIdDetails'

-- | An entirely unhelpful 'GlobalIdDetails'
notGlobalId :: GlobalIdDetails
notGlobalId = NotGlobalId

instance Outputable GlobalIdDetails where
    ppr NotGlobalId       = ptext (sLit "[***NotGlobalId***]")
    ppr VanillaGlobal     = ptext (sLit "[GlobalId]")
    ppr (DataConWorkId _) = ptext (sLit "[DataCon]")
    ppr (DataConWrapId _) = ptext (sLit "[DataConWrapper]")
    ppr (ClassOpId _)     = ptext (sLit "[ClassOp]")
    ppr (PrimOpId _)      = ptext (sLit "[PrimOp]")
    ppr (FCallId _)       = ptext (sLit "[ForeignCall]")
    ppr (TickBoxOpId _)   = ptext (sLit "[TickBoxOp]")
    ppr (RecordSelId {})  = ptext (sLit "[RecSel]")
\end{code}


%************************************************************************
%*									*
\subsection{The main IdInfo type}
%*									*
%************************************************************************

\begin{code}
-- | An 'IdInfo' gives /optional/ information about an 'Id'.  If
-- present it never lies, but it may not be present, in which case there
-- is always a conservative assumption which can be made.
-- 
-- Two 'Id's may have different info even though they have the same
-- 'Unique' (and are hence the same 'Id'); for example, one might lack
-- the properties attached to the other.
-- 
-- The 'IdInfo' gives information about the value, or definition, of the
-- 'Id'.  It does not contain information about the 'Id''s usage,
-- except for 'demandInfo' and 'lbvarInfo'.
data IdInfo
  = IdInfo {
	arityInfo 	:: !ArityInfo,		-- ^ 'Id' arity
	specInfo 	:: SpecInfo,		-- ^ Specialisations of the 'Id's function which exist
#ifdef OLD_STRICTNESS
	cprInfo 	:: CprInfo,             -- ^ If the 'Id's function always constructs a product result
	demandInfo 	:: Demand.Demand,	-- ^ Whether or not the 'Id' is definitely demanded
	strictnessInfo	:: StrictnessInfo,	-- ^ 'Id' strictness properties
#endif
        workerInfo      :: WorkerInfo,          -- ^ Pointer to worker function.
						-- Within one module this is irrelevant; the 
						-- inlining of a worker is handled via the 'Unfolding'.
						-- However, when the module is imported by others, the
						-- 'WorkerInfo' is used /only/ to indicate the form of
						-- the RHS, so that interface files don't actually 
						-- need to contain the RHS; it can be derived from
						-- the strictness info

	unfoldingInfo	:: Unfolding,		-- ^ The 'Id's unfolding
	cafInfo		:: CafInfo,		-- ^ 'Id' CAF info
        lbvarInfo	:: LBVarInfo,		-- ^ Info about a lambda-bound variable, if the 'Id' is one
	inlinePragInfo	:: InlinePragInfo,	-- ^ Any inline pragma atached to the 'Id'
	occInfo		:: OccInfo,		-- ^ How the 'Id' occurs in the program

	newStrictnessInfo :: Maybe StrictSig,	-- ^ Id strictness information. Reason for Maybe: 
	                                        -- the DmdAnal phase needs to know whether
						-- this is the first visit, so it can assign botSig.
						-- Other customers want topSig.  So @Nothing@ is good.

	newDemandInfo	  :: Maybe Demand	-- ^ Id demand information. Similarly we want to know 
	                                        -- if there's no known demand yet, for when we are looking
						-- for CPR info
    }

-- | Just evaluate the 'IdInfo' to WHNF
seqIdInfo :: IdInfo -> ()
seqIdInfo (IdInfo {}) = ()

-- | Evaluate all the fields of the 'IdInfo' that are generally demanded by the
-- compiler
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
setWorkerInfo :: IdInfo -> WorkerInfo -> IdInfo
setWorkerInfo     info wk = wk `seq` info { workerInfo = wk }
setSpecInfo :: IdInfo -> SpecInfo -> IdInfo
setSpecInfo 	  info sp = sp `seq` info { specInfo = sp }
setInlinePragInfo :: IdInfo -> InlinePragInfo -> IdInfo
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo :: IdInfo -> OccInfo -> IdInfo
setOccInfo	  info oc = oc `seq` info { occInfo = oc }
#ifdef OLD_STRICTNESS
setStrictnessInfo info st = st `seq` info { strictnessInfo = st }
#endif
	-- Try to avoid spack leaks by seq'ing

setUnfoldingInfoLazily :: IdInfo -> Unfolding -> IdInfo
setUnfoldingInfoLazily info uf 	-- Lazy variant to avoid looking at the
  =				-- unfolding of an imported Id unless necessary
    info { unfoldingInfo = uf }	-- (In this case the demand-zapping is redundant.)

setUnfoldingInfo :: IdInfo -> Unfolding -> IdInfo
setUnfoldingInfo info uf 
	-- We do *not* seq on the unfolding info, For some reason, doing so 
	-- actually increases residency significantly. 
  = info { unfoldingInfo = uf }

#ifdef OLD_STRICTNESS
setDemandInfo	  info dd = info { demandInfo = dd }
setCprInfo        info cp = info { cprInfo = cp }
#endif

setArityInfo :: IdInfo -> ArityInfo -> IdInfo
setArityInfo	  info ar  = info { arityInfo = ar  }
setCafInfo :: IdInfo -> CafInfo -> IdInfo
setCafInfo        info caf = info { cafInfo = caf }

setLBVarInfo :: IdInfo -> LBVarInfo -> IdInfo
setLBVarInfo      info lb = {-lb `seq`-} info { lbvarInfo = lb }

setNewDemandInfo :: IdInfo -> Maybe Demand -> IdInfo
setNewDemandInfo     info dd = dd `seq` info { newDemandInfo = dd }
setNewStrictnessInfo :: IdInfo -> Maybe StrictSig -> IdInfo
setNewStrictnessInfo info dd = dd `seq` info { newStrictnessInfo = dd }
\end{code}


\begin{code}
-- | Basic 'IdInfo' that carries no useful information whatsoever
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

-- | More informative 'IdInfo' we can use when we know the 'Id' has no CAF references
noCafIdInfo :: IdInfo
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
-- | An 'ArityInfo' of @n@ tells us that partial application of this 
-- 'Id' to up to @n-1@ value arguments does essentially no work.
--
-- That is not necessarily the same as saying that it has @n@ leading 
-- lambdas, because coerces may get in the way.
--
-- The arity might increase later in the compilation process, if
-- an extra lambda floats up to the binding site.
type ArityInfo = Arity

-- | It is always safe to assume that an 'Id' has an arity of 0
unknownArity :: Arity
unknownArity = 0 :: Arity

ppArityInfo :: Int -> SDoc
ppArityInfo 0 = empty
ppArityInfo n = hsep [ptext (sLit "Arity"), int n]
\end{code}

%************************************************************************
%*									*
\subsection{Inline-pragma information}
%*									*
%************************************************************************

\begin{code}
-- | Tells when the inlining is active.
-- When it is active the thing may be inlined, depending on how
-- big it is.
--
-- If there was an @INLINE@ pragma, then as a separate matter, the
-- RHS will have been made to look small with a Core inline 'Note'
--
-- The default 'InlinePragInfo' is 'AlwaysActive', so the info serves
-- entirely as a way to inhibit inlining until we want it
type InlinePragInfo = Activation
\end{code}


%************************************************************************
%*									*
	SpecInfo
%*									*
%************************************************************************

\begin{code}
-- | Records the specializations of this 'Id' that we know about
-- in the form of rewrite 'CoreRule's that target them
data SpecInfo 
  = SpecInfo 
	[CoreRule] 
	VarSet		-- Locally-defined free vars of *both* LHS and RHS 
			-- of rules.  I don't think it needs to include the
			-- ru_fn though.
			-- Note [Rule dependency info] in OccurAnal

-- | Assume that no specilizations exist: always safe
emptySpecInfo :: SpecInfo
emptySpecInfo = SpecInfo [] emptyVarSet

isEmptySpecInfo :: SpecInfo -> Bool
isEmptySpecInfo (SpecInfo rs _) = null rs

-- | Retrieve the locally-defined free variables of both the left and
-- right hand sides of the specialization rules
specInfoFreeVars :: SpecInfo -> VarSet
specInfoFreeVars (SpecInfo _ fvs) = fvs

specInfoRules :: SpecInfo -> [CoreRule]
specInfoRules (SpecInfo rules _) = rules

-- | Change the name of the function the rule is keyed on on all of the 'CoreRule's
setSpecInfoHead :: Name -> SpecInfo -> SpecInfo
setSpecInfoHead fn (SpecInfo rules fvs)
  = SpecInfo (map (setRuleIdName fn) rules) fvs

seqSpecInfo :: SpecInfo -> ()
seqSpecInfo (SpecInfo rules fvs) = seqRules rules `seq` seqVarSet fvs
\end{code}

%************************************************************************
%*									*
\subsection[worker-IdInfo]{Worker info about an @Id@}
%*									*
%************************************************************************

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

-- | If this Id has a worker then we store a reference to it. Worker
-- functions are generated by the worker\/wrapper pass, using information
-- information from strictness analysis.
data WorkerInfo = NoWorker              -- ^ No known worker function
		| HasWorker Id Arity    -- ^ The 'Arity' is the arity of the /wrapper/ at the moment of the
	                                -- worker\/wrapper split, which may be different from the current 'Id' 'Aritiy'

seqWorker :: WorkerInfo -> ()
seqWorker (HasWorker id a) = id `seq` a `seq` ()
seqWorker NoWorker	   = ()

ppWorkerInfo :: WorkerInfo -> SDoc
ppWorkerInfo NoWorker            = empty
ppWorkerInfo (HasWorker wk_id _) = ptext (sLit "Worker") <+> ppr wk_id

workerExists :: WorkerInfo -> Bool
workerExists NoWorker        = False
workerExists (HasWorker _ _) = True

-- | The 'Id' of the worker function if it exists, or a panic otherwise
workerId :: WorkerInfo -> Id
workerId (HasWorker id _) = id
workerId NoWorker = panic "workerId: NoWorker"

-- | The 'Arity' of the worker function at the time of the split if it exists, or a panic otherwise
wrapperArity :: WorkerInfo -> Arity
wrapperArity (HasWorker _ a) = a
wrapperArity NoWorker = panic "wrapperArity: NoWorker"
\end{code}


%************************************************************************
%*									*
\subsection[CG-IdInfo]{Code generator-related information}
%*									*
%************************************************************************

\begin{code}
-- CafInfo is used to build Static Reference Tables (see simplStg/SRT.lhs).

-- | Records whether an 'Id' makes Constant Applicative Form references
data CafInfo 
	= MayHaveCafRefs		-- ^ Indicates that the 'Id' is for either:
					--
					-- 1. A function or static constructor
					--    that refers to one or more CAFs, or
					--
					-- 2. A real live CAF

	| NoCafRefs			-- ^ A function or static constructor
				        -- that refers to no CAFs.

-- | Assumes that the 'Id' has CAF references: definitely safe
vanillaCafInfo :: CafInfo
vanillaCafInfo = MayHaveCafRefs

mayHaveCafRefs :: CafInfo -> Bool
mayHaveCafRefs  MayHaveCafRefs = True
mayHaveCafRefs _	       = False

seqCaf :: CafInfo -> ()
seqCaf c = c `seq` ()

ppCafInfo :: CafInfo -> SDoc
ppCafInfo NoCafRefs = ptext (sLit "NoCafRefs")
ppCafInfo MayHaveCafRefs = empty
\end{code}

%************************************************************************
%*									*
\subsection[cpr-IdInfo]{Constructed Product Result info about an @Id@}
%*									*
%************************************************************************

\begin{code}
#ifdef OLD_STRICTNESS
-- | If the @Id@ is a function then it may have Constructed Product Result 
-- (CPR) info. A CPR analysis phase detects whether:
-- 
-- 1. The function's return value has a product type, i.e. an algebraic  type 
-- with a single constructor. Examples of such types are tuples and boxed
-- primitive values.
--
-- 2. The function always 'constructs' the value that it is returning.  It
-- must do this on every path through,  and it's OK if it calls another
-- function which constructs the result.
-- 
-- If this is the case then we store a template which tells us the
-- function has the CPR property and which components of the result are
-- also CPRs.
data CprInfo
  = NoCPRInfo   -- ^ No, this function does not return a constructed product
  | ReturnsCPR	-- ^ Yes, this function returns a constructed product
		
		-- Implicitly, this means "after the function has been applied
		-- to all its arguments", so the worker\/wrapper builder in 
		-- WwLib.mkWWcpr checks that that it is indeed saturated before
		-- making use of the CPR info

	-- We used to keep nested info about sub-components, but
	-- we never used it so I threw it away

-- | It's always safe to assume that an 'Id' does not have the CPR property
noCprInfo :: CprInt
noCprInfo = NoCPRInfo

seqCpr :: CprInfo -> ()
seqCpr ReturnsCPR = ()
seqCpr NoCPRInfo  = ()

ppCprInfo NoCPRInfo  = empty
ppCprInfo ReturnsCPR = ptext (sLit "__M")

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

\begin{code}
-- | If the 'Id' is a lambda-bound variable then it may have lambda-bound
-- variable info. Sometimes we know whether the lambda binding this variable
-- is a \"one-shot\" lambda; that is, whether it is applied at most once.
--
-- This information may be useful in optimisation, as computations may
-- safely be floated inside such a lambda without risk of duplicating
-- work.
data LBVarInfo = NoLBVarInfo            -- ^ No information
	       | IsOneShotLambda	-- ^ The lambda is applied at most once).

-- | It is always safe to assume that an 'Id' has no lambda-bound variable information
noLBVarInfo :: LBVarInfo
noLBVarInfo = NoLBVarInfo

hasNoLBVarInfo :: LBVarInfo -> Bool
hasNoLBVarInfo NoLBVarInfo     = True
hasNoLBVarInfo IsOneShotLambda = False

seqLBVar :: LBVarInfo -> ()
seqLBVar l = l `seq` ()

pprLBVarInfo :: LBVarInfo -> SDoc
pprLBVarInfo NoLBVarInfo     = empty
pprLBVarInfo IsOneShotLambda = ptext (sLit "OneShot")

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

\begin{code}
-- | This is used to remove information on lambda binders that we have
-- setup as part of a lambda group, assuming they will be applied all at once,
-- but turn out to be part of an unsaturated lambda as in e.g:
--
-- > (\x1. \x2. e) arg1
zapLamInfo :: IdInfo -> Maybe IdInfo
zapLamInfo info@(IdInfo {occInfo = occ, newDemandInfo = demand})
  | is_safe_occ occ && is_safe_dmd demand
  = Nothing
  | otherwise
  = Just (info {occInfo = safe_occ, newDemandInfo = Nothing})
  where
	-- The "unsafe" occ info is the ones that say I'm not in a lambda
	-- because that might not be true for an unsaturated lambda
    is_safe_occ (OneOcc in_lam _ _) = in_lam
    is_safe_occ _other		    = True

    safe_occ = case occ of
		 OneOcc _ once int_cxt -> OneOcc insideLam once int_cxt
		 _other	       	       -> occ

    is_safe_dmd Nothing    = True
    is_safe_dmd (Just dmd) = not (isStrictDmd dmd)
\end{code}

\begin{code}
-- | Remove demand info on the 'IdInfo' if it is present, otherwise return @Nothing@
zapDemandInfo :: IdInfo -> Maybe IdInfo
zapDemandInfo info@(IdInfo {newDemandInfo = dmd})
  | isJust dmd = Just (info {newDemandInfo = Nothing})
  | otherwise  = Nothing
\end{code}

\begin{code}
zapFragileInfo :: IdInfo -> Maybe IdInfo
-- ^ Zap info that depends on free variables
zapFragileInfo info 
  = Just (info `setSpecInfo` emptySpecInfo
	       `setWorkerInfo` NoWorker
               `setUnfoldingInfo` noUnfolding
	       `setOccInfo` if isFragileOcc occ then NoOccInfo else occ)
  where
    occ = occInfo info
\end{code}

%************************************************************************
%*									*
\subsection{TickBoxOp}
%*									*
%************************************************************************

\begin{code}
type TickBoxId = Int

-- | Tick box for Hpc-style coverage
data TickBoxOp 
   = TickBox Module {-# UNPACK #-} !TickBoxId

instance Outputable TickBoxOp where
    ppr (TickBox mod n)         = ptext (sLit "tick") <+> ppr (mod,n)
\end{code}
