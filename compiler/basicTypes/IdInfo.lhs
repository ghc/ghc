%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module IdInfo (
        -- * The IdDetails type
	IdDetails(..), pprIdDetails, coVarDetails,

        -- * The IdInfo type
	IdInfo,		-- Abstract
	vanillaIdInfo, noCafIdInfo,
	seqIdInfo, megaSeqIdInfo,

	-- ** Zapping various forms of Info
	zapLamInfo, zapFragileInfo,

        zapDemandInfo,

	-- ** The ArityInfo type
	ArityInfo,
	unknownArity, 
	arityInfo, setArityInfo, ppArityInfo, 

	-- ** Demand and strictness Info
 	strictnessInfo, setStrictnessInfo, 
  	demandInfo, setDemandInfo, pprStrictness,

	-- ** Unfolding Info
	unfoldingInfo, setUnfoldingInfo, setUnfoldingInfoLazily,

	-- ** The InlinePragInfo type
	InlinePragInfo,
	inlinePragInfo, setInlinePragInfo,

	-- ** The OccInfo type
	OccInfo(..),
	isDeadOcc, isStrongLoopBreaker, isWeakLoopBreaker,
	occInfo, setOccInfo,

	InsideLam, OneBranch,
	insideLam, notInsideLam, oneBranch, notOneBranch,
	
	-- ** The SpecInfo type
	SpecInfo(..),
	emptySpecInfo,
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

import CoreSyn

import Class
import {-# SOURCE #-} PrimOp (PrimOp)
import Name
import VarSet
import BasicTypes
import DataCon
import TyCon
import ForeignCall
import Outputable	
import Module
import FastString
import Demand

-- infixl so you can say (id `set` a `set` b)
infixl 	1 `setSpecInfo`,
	  `setArityInfo`,
	  `setInlinePragInfo`,
	  `setUnfoldingInfo`,
	  `setLBVarInfo`,
	  `setOccInfo`,
	  `setCafInfo`,
	  `setStrictnessInfo`,
	  `setDemandInfo`
\end{code}

%************************************************************************
%*									*
                     IdDetails
%*									*
%************************************************************************

\begin{code}
-- | The 'IdDetails' of an 'Id' give stable, and necessary, 
-- information about the Id. 
data IdDetails
  = VanillaId	

  -- | The 'Id' for a record selector
  | RecSelId                 
    { sel_tycon   :: TyCon	-- ^ For a data type family, this is the /instance/ 'TyCon'
				--   not the family 'TyCon'
    , sel_naughty :: Bool       -- True <=> a "naughty" selector which can't actually exist, for example @x@ in:
                                --    data T = forall a. MkT { x :: a }
    }				-- See Note [Naughty record selectors] in TcTyClsDecls

  | DataConWorkId DataCon	-- ^ The 'Id' is for a data constructor /worker/
  | DataConWrapId DataCon	-- ^ The 'Id' is for a data constructor /wrapper/
				
				-- [the only reasons we need to know is so that
				--  a) to support isImplicitId
				--  b) when desugaring a RecordCon we can get 
				--     from the Id back to the data con]

  | ClassOpId Class 		-- ^ The 'Id' is a superclass selector or class operation of a class

  | PrimOpId PrimOp		-- ^ The 'Id' is for a primitive operator
  | FCallId ForeignCall		-- ^ The 'Id' is for a foreign call

  | TickBoxOpId TickBoxOp	-- ^ The 'Id' is for a HPC tick box (both traditional and binary)

  | DFunId Int Bool             -- ^ A dictionary function.
       -- Int = the number of "silent" arguments to the dfun
       --       e.g.  class D a => C a where ...
       --             instance C a => C [a]
       --       has is_silent = 1, because the dfun
       --       has type  dfun :: (D a, C a) => C [a]
       --       See Note [Silent superclass arguments] in TcInstDcls
       --
       -- Bool = True <=> the class has only one method, so may be
       --                  implemented with a newtype, so it might be bad
       --                  to be strict on this dictionary

coVarDetails :: IdDetails
coVarDetails = VanillaId

instance Outputable IdDetails where
    ppr = pprIdDetails

pprIdDetails :: IdDetails -> SDoc
pprIdDetails VanillaId = empty
pprIdDetails other     = brackets (pp other)
 where
   pp VanillaId         = panic "pprIdDetails"
   pp (DataConWorkId _) = ptext (sLit "DataCon")
   pp (DataConWrapId _) = ptext (sLit "DataConWrapper")
   pp (ClassOpId {})    = ptext (sLit "ClassOp")
   pp (PrimOpId _)      = ptext (sLit "PrimOp")
   pp (FCallId _)       = ptext (sLit "ForeignCall")
   pp (TickBoxOpId _)   = ptext (sLit "TickBoxOp")
   pp (DFunId ns nt)    = ptext (sLit "DFunId")
                             <> ppWhen (ns /= 0) (brackets (int ns))
                             <> ppWhen nt (ptext (sLit "(nt)"))
   pp (RecSelId { sel_naughty = is_naughty })
      			 = brackets $ ptext (sLit "RecSel") 
      			    <> ppWhen is_naughty (ptext (sLit "(naughty)"))
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
			   			-- See Note [Specialisations and RULES in IdInfo]
	unfoldingInfo	:: Unfolding,		-- ^ The 'Id's unfolding
	cafInfo		:: CafInfo,		-- ^ 'Id' CAF info
        lbvarInfo	:: LBVarInfo,		-- ^ Info about a lambda-bound variable, if the 'Id' is one
	inlinePragInfo	:: InlinePragma,	-- ^ Any inline pragma atached to the 'Id'
	occInfo		:: OccInfo,		-- ^ How the 'Id' occurs in the program

        strictnessInfo  :: StrictSig,      --  ^ A strictness signature

        demandInfo      :: Demand        -- ^ ID demand information

    }

-- | Just evaluate the 'IdInfo' to WHNF
seqIdInfo :: IdInfo -> ()
seqIdInfo (IdInfo {}) = ()

-- | Evaluate all the fields of the 'IdInfo' that are generally demanded by the
-- compiler
megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqSpecInfo (specInfo info)			`seq`

-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all
--    seqUnfolding (unfoldingInfo info)		`seq`

    seqDemandInfo (demandInfo info)         `seq`
    seqStrictnessInfo (strictnessInfo info) `seq`

    seqCaf (cafInfo info)			`seq`
    seqLBVar (lbvarInfo info)			`seq`
    seqOccInfo (occInfo info) 

seqStrictnessInfo :: StrictSig -> ()
seqStrictnessInfo ty = seqStrictSig ty

seqDemandInfo :: Demand -> ()
seqDemandInfo dmd = seqDemand dmd
\end{code}

Setters

\begin{code}
setSpecInfo :: IdInfo -> SpecInfo -> IdInfo
setSpecInfo 	  info sp = sp `seq` info { specInfo = sp }
setInlinePragInfo :: IdInfo -> InlinePragma -> IdInfo
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo :: IdInfo -> OccInfo -> IdInfo
setOccInfo	  info oc = oc `seq` info { occInfo = oc }
	-- Try to avoid spack leaks by seq'ing

setUnfoldingInfoLazily :: IdInfo -> Unfolding -> IdInfo
setUnfoldingInfoLazily info uf 	-- Lazy variant to avoid looking at the
  =				-- unfolding of an imported Id unless necessary
    info { unfoldingInfo = uf }	-- (In this case the demand-zapping is redundant.)

setUnfoldingInfo :: IdInfo -> Unfolding -> IdInfo
setUnfoldingInfo info uf 
  = -- We don't seq the unfolding, as we generate intermediate
    -- unfoldings which are just thrown away, so evaluating them is a
    -- waste of time.
    -- seqUnfolding uf `seq`
    info { unfoldingInfo = uf }

setArityInfo :: IdInfo -> ArityInfo -> IdInfo
setArityInfo	  info ar  = info { arityInfo = ar  }
setCafInfo :: IdInfo -> CafInfo -> IdInfo
setCafInfo        info caf = info { cafInfo = caf }

setLBVarInfo :: IdInfo -> LBVarInfo -> IdInfo
setLBVarInfo      info lb = {-lb `seq`-} info { lbvarInfo = lb }

setDemandInfo :: IdInfo -> Demand -> IdInfo
setDemandInfo info dd = dd `seq` info { demandInfo = dd }

setStrictnessInfo :: IdInfo -> StrictSig -> IdInfo
setStrictnessInfo info dd = dd `seq` info { strictnessInfo = dd }
\end{code}


\begin{code}
-- | Basic 'IdInfo' that carries no useful information whatsoever
vanillaIdInfo :: IdInfo
vanillaIdInfo 
  = IdInfo {
	    cafInfo		= vanillaCafInfo,
	    arityInfo		= unknownArity,
	    specInfo		= emptySpecInfo,
	    unfoldingInfo	= noUnfolding,
	    lbvarInfo		= NoLBVarInfo,
	    inlinePragInfo 	= defaultInlinePragma,
	    occInfo		= NoOccInfo,
            demandInfo	        = topDmd,
	    strictnessInfo      = topSig
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
type InlinePragInfo = InlinePragma
\end{code}


%************************************************************************
%*									*
               Strictness
%*									*
%************************************************************************

\begin{code}
pprStrictness :: StrictSig -> SDoc
pprStrictness sig = ppr sig
\end{code}


%************************************************************************
%*									*
	SpecInfo
%*									*
%************************************************************************

Note [Specialisations and RULES in IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, a GlobalIdshas an *empty* SpecInfo.  All their
RULES are contained in the globally-built rule-base.  In principle,
one could attach the to M.f the RULES for M.f that are defined in M.
But we don't do that for instance declarations and so we just treat
them all uniformly.

The EXCEPTION is PrimOpIds, which do have rules in their IdInfo. That is
jsut for convenience really.

However, LocalIds may have non-empty SpecInfo.  We treat them 
differently because:
  a) they might be nested, in which case a global table won't work
  b) the RULE might mention free variables, which we use to keep things alive

In TidyPgm, when the LocalId becomes a GlobalId, its RULES are stripped off
and put in the global list.

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
        deriving (Eq, Ord)

-- | Assumes that the 'Id' has CAF references: definitely safe
vanillaCafInfo :: CafInfo
vanillaCafInfo = MayHaveCafRefs

mayHaveCafRefs :: CafInfo -> Bool
mayHaveCafRefs  MayHaveCafRefs = True
mayHaveCafRefs _	       = False

seqCaf :: CafInfo -> ()
seqCaf c = c `seq` ()

instance Outputable CafInfo where
   ppr = ppCafInfo

ppCafInfo :: CafInfo -> SDoc
ppCafInfo NoCafRefs = ptext (sLit "NoCafRefs")
ppCafInfo MayHaveCafRefs = empty
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
zapLamInfo info@(IdInfo {occInfo = occ, demandInfo = demand})
  | is_safe_occ occ && is_safe_dmd demand
  = Nothing
  | otherwise
  = Just (info {occInfo = safe_occ, demandInfo = topDmd})
  where
	-- The "unsafe" occ info is the ones that say I'm not in a lambda
	-- because that might not be true for an unsaturated lambda
    is_safe_occ (OneOcc in_lam _ _) = in_lam
    is_safe_occ _other		    = True

    safe_occ = case occ of
		 OneOcc _ once int_cxt -> OneOcc insideLam once int_cxt
		 _other	       	       -> occ

    is_safe_dmd dmd = not (isStrictDmd dmd)
\end{code}

\begin{code}
-- | Remove demand info on the 'IdInfo' if it is present, otherwise return @Nothing@
zapDemandInfo :: IdInfo -> Maybe IdInfo
zapDemandInfo info = Just (info {demandInfo = topDmd})
\end{code}

\begin{code}
zapFragileInfo :: IdInfo -> Maybe IdInfo
-- ^ Zap info that depends on free variables
zapFragileInfo info 
  = Just (info `setSpecInfo` emptySpecInfo
               `setUnfoldingInfo` noUnfolding
	       `setOccInfo` zapFragileOcc occ)
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
