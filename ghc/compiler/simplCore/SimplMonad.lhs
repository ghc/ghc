%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplMonad (
	InId, InBind, InExpr, InAlt, InArg, InType, InBinder,
	OutId, OutTyVar, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBinder,
	FloatsWith, FloatsWithExpr,

	-- The monad
	SimplM,
	initSmpl, returnSmpl, thenSmpl, thenSmpl_,
	mapSmpl, mapAndUnzipSmpl, mapAccumLSmpl,
	getDOptsSmpl,

	-- The simplifier mode
	setMode, getMode, 

        -- Unique supply
        getUniqueSmpl, getUniquesSmpl, getUniqSupplySmpl,

	-- Counting
	SimplCount, Tick(..),
	tick, freeTick,
	getSimplCount, zeroSimplCount, pprSimplCount, 
	plusSimplCount, isZeroSimplCount,

	-- Switch checker
	SwitchChecker, SwitchResult(..), getSwitchChecker, getSimplIntSwitch,
	isAmongSimpl, intSwitchSet, switchIsOn,

	-- Cost centres
	getEnclosingCC, setEnclosingCC,

	-- Environments
	SimplEnv, emptySimplEnv, getSubst, setSubst,
	getSubstEnv, extendSubst, extendSubstList,
	getInScope, setInScope, modifyInScope, addNewInScopeIds,
	setSubstEnv, zapSubstEnv,

	-- Floats
  	Floats, emptyFloats, isEmptyFloats, unitFloat, addFloats, flattenFloats,
	allLifted, wrapFloats, floatBinds,
	addAuxiliaryBind,

	-- Inlining,
	preInlineUnconditionally, postInlineUnconditionally, activeInline, activeRule,
	inlineMode
    ) where

#include "HsVersions.h"

import Id		( Id, idType, idOccInfo, idInlinePragma	)
import CoreSyn
import CoreUtils	( needsCaseBinding, exprIsTrivial )
import PprCore		()	-- Instances
import CostCentre	( CostCentreStack, subsumedCCS )
import Var	
import VarEnv
import VarSet
import OrdList
import qualified Subst
import Subst		( Subst, mkSubst, substEnv, 
			  InScopeSet, mkInScopeSet, substInScope,
			  isInScope 
			)
import Type             ( Type, isUnLiftedType )
import UniqSupply	( uniqsFromSupply, uniqFromSupply, splitUniqSupply,
			  UniqSupply
			)
import FiniteMap
import BasicTypes	( TopLevelFlag, isTopLevel, isLoopBreaker,
			  Activation, isActive, isAlwaysActive,
			  OccInfo(..), isOneOcc
			)
import CmdLineOpts	( SimplifierSwitch(..), SimplifierMode(..),
			  DynFlags, DynFlag(..), dopt, 
			  opt_PprStyle_Debug, opt_HistorySize, opt_SimplNoPreInlining, opt_RulesOff
			)
import Unique		( Unique )
import Outputable
import FastTypes
import FastString
import Maybes		( expectJust )

import GLAEXTS		( indexArray# )

#if __GLASGOW_HASKELL__ < 503
import PrelArr  ( Array(..) )
#else
import GHC.Arr  ( Array(..) )
#endif

import Array		( array, (//) )

infixr 0  `thenSmpl`, `thenSmpl_`
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InBinder  = CoreBndr
type InId      = Id			-- Not yet cloned
type InType    = Type			-- Ditto
type InBind    = CoreBind
type InExpr    = CoreExpr
type InAlt     = CoreAlt
type InArg     = CoreArg

type OutBinder  = CoreBndr
type OutId	= Id			-- Cloned
type OutTyVar	= TyVar			-- Cloned
type OutType	= Type			-- Cloned
type OutBind	= CoreBind
type OutExpr	= CoreExpr
type OutAlt	= CoreAlt
type OutArg	= CoreArg
\end{code}

%************************************************************************
%*									*
\subsection{Floats}
%*									*
%************************************************************************

\begin{code}
type FloatsWithExpr = FloatsWith OutExpr
type FloatsWith a   = (Floats, a)
	-- We return something equivalent to (let b in e), but
	-- in pieces to avoid the quadratic blowup when floating 
	-- incrementally.  Comments just before simplExprB in Simplify.lhs

data Floats = Floats (OrdList OutBind) 
		     InScopeSet		-- Environment "inside" all the floats
		     Bool		-- True <=> All bindings are lifted

allLifted :: Floats -> Bool
allLifted (Floats _ _ is_lifted) = is_lifted

wrapFloats :: Floats -> OutExpr -> OutExpr
wrapFloats (Floats bs _ _) body = foldrOL Let body bs

isEmptyFloats :: Floats -> Bool
isEmptyFloats (Floats bs _ _) = isNilOL bs 

floatBinds :: Floats -> [OutBind]
floatBinds (Floats bs _ _) = fromOL bs

flattenFloats :: Floats -> Floats
-- Flattens into a single Rec group
flattenFloats (Floats bs is is_lifted) 
  = ASSERT2( is_lifted, ppr (fromOL bs) )
    Floats (unitOL (Rec (flattenBinds (fromOL bs)))) is is_lifted
\end{code}

\begin{code}
emptyFloats :: SimplEnv -> Floats
emptyFloats env = Floats nilOL (getInScope env) True

unitFloat :: SimplEnv -> OutId -> OutExpr -> Floats
-- A single non-rec float; extend the in-scope set
unitFloat env var rhs = Floats (unitOL (NonRec var rhs))
			       (Subst.extendInScopeSet (getInScope env) var)
			       (not (isUnLiftedType (idType var)))

addFloats :: SimplEnv -> Floats 
	  -> (SimplEnv -> SimplM (FloatsWith a))
	  -> SimplM (FloatsWith a)
addFloats env (Floats b1 is1 l1) thing_inside
  | isNilOL b1 
  = thing_inside env
  | otherwise
  = thing_inside (setInScopeSet env is1)	`thenSmpl` \ (Floats b2 is2 l2, res) ->
    returnSmpl (Floats (b1 `appOL` b2) is2 (l1 && l2), res)

addLetBind :: OutBind -> Floats -> Floats
addLetBind bind (Floats binds in_scope lifted) 
  = Floats (bind `consOL` binds) in_scope (lifted && is_lifted_bind bind)

is_lifted_bind (Rec _)      = True
is_lifted_bind (NonRec b r) = not (isUnLiftedType (idType b))

-- addAuxiliaryBind 	* takes already-simplified things (bndr and rhs)
--			* extends the in-scope env
--			* assumes it's a let-bindable thing
addAuxiliaryBind :: SimplEnv -> OutBind
		 -> (SimplEnv -> SimplM (FloatsWith a))
	 	 -> SimplM (FloatsWith a)
	-- Extends the in-scope environment as well as wrapping the bindings
addAuxiliaryBind env bind thing_inside
  = ASSERT( case bind of { NonRec b r -> not (needsCaseBinding (idType b) r) ; Rec _ -> True } )
    thing_inside (addNewInScopeIds env (bindersOf bind))	`thenSmpl` \ (floats, x) ->
    returnSmpl (addLetBind bind floats, x)
\end{code}


%************************************************************************
%*									*
\subsection{Monad plumbing}
%*									*
%************************************************************************

For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)

\begin{code}
type SimplM result
  =  DynFlags		-- We thread the unique supply because
  -> UniqSupply		-- constantly splitting it is rather expensive
  -> SimplCount 
  -> (result, UniqSupply, SimplCount)
\end{code}

\begin{code}
initSmpl :: DynFlags
	 -> UniqSupply		-- No init count; set to 0
	 -> SimplM a
	 -> (a, SimplCount)

initSmpl dflags us m
  = case m dflags us (zeroSimplCount dflags) of 
	(result, _, count) -> (result, count)


{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}

returnSmpl :: a -> SimplM a
returnSmpl e dflags us sc = (e, us, sc)

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k dflags us0 sc0
  = case (m dflags us0 sc0) of 
	(m_result, us1, sc1) -> k m_result dflags us1 sc1

thenSmpl_ m k dflags us0 sc0
  = case (m dflags us0 sc0) of 
	(_, us1, sc1) -> k dflags us1 sc1
\end{code}


\begin{code}
mapSmpl	    	:: (a -> SimplM b) -> [a] -> SimplM [b]
mapAndUnzipSmpl :: (a -> SimplM (b, c)) -> [a] -> SimplM ([b],[c])

mapSmpl f [] = returnSmpl []
mapSmpl f (x:xs)
  = f x		    `thenSmpl` \ x'  ->
    mapSmpl f xs    `thenSmpl` \ xs' ->
    returnSmpl (x':xs')

mapAndUnzipSmpl f [] = returnSmpl ([],[])
mapAndUnzipSmpl f (x:xs)
  = f x			    `thenSmpl` \ (r1,  r2)  ->
    mapAndUnzipSmpl f xs    `thenSmpl` \ (rs1, rs2) ->
    returnSmpl (r1:rs1, r2:rs2)

mapAccumLSmpl f acc []     = returnSmpl (acc, [])
mapAccumLSmpl f acc (x:xs) = f acc x	`thenSmpl` \ (acc', x') ->
			     mapAccumLSmpl f acc' xs	`thenSmpl` \ (acc'', xs') ->
			     returnSmpl (acc'', x':xs')
\end{code}


%************************************************************************
%*									*
\subsection{The unique supply}
%*									*
%************************************************************************

\begin{code}
getUniqSupplySmpl :: SimplM UniqSupply
getUniqSupplySmpl dflags us sc 
   = case splitUniqSupply us of
        (us1, us2) -> (us1, us2, sc)

getUniqueSmpl :: SimplM Unique
getUniqueSmpl dflags us sc 
   = case splitUniqSupply us of
        (us1, us2) -> (uniqFromSupply us1, us2, sc)

getUniquesSmpl :: SimplM [Unique]
getUniquesSmpl dflags us sc 
   = case splitUniqSupply us of
        (us1, us2) -> (uniqsFromSupply us1, us2, sc)

getDOptsSmpl :: SimplM DynFlags
getDOptsSmpl dflags us sc 
   = (dflags, us, sc)
\end{code}


%************************************************************************
%*									*
\subsection{Counting up what we've done}
%*									*
%************************************************************************

\begin{code}
getSimplCount :: SimplM SimplCount
getSimplCount dflags us sc = (sc, us, sc)

tick :: Tick -> SimplM ()
tick t dflags us sc 
   = sc' `seq` ((), us, sc')
     where
        sc' = doTick t sc

freeTick :: Tick -> SimplM ()
-- Record a tick, but don't add to the total tick count, which is
-- used to decide when nothing further has happened
freeTick t dflags us sc 
   = sc' `seq` ((), us, sc')
        where
           sc' = doFreeTick t sc
\end{code}

\begin{code}
verboseSimplStats = opt_PprStyle_Debug		-- For now, anyway

zeroSimplCount	   :: DynFlags -> SimplCount
isZeroSimplCount   :: SimplCount -> Bool
pprSimplCount	   :: SimplCount -> SDoc
doTick, doFreeTick :: Tick -> SimplCount -> SimplCount
plusSimplCount     :: SimplCount -> SimplCount -> SimplCount
\end{code}

\begin{code}
data SimplCount = VerySimplZero		-- These two are used when 
		| VerySimplNonZero	-- we are only interested in 
					-- termination info

		| SimplCount	{
			ticks   :: !Int,		-- Total ticks
			details :: !TickCounts,		-- How many of each type
			n_log	:: !Int,		-- N
			log1	:: [Tick],		-- Last N events; <= opt_HistorySize
			log2	:: [Tick]		-- Last opt_HistorySize events before that
		  }

type TickCounts = FiniteMap Tick Int

zeroSimplCount dflags
		-- This is where we decide whether to do
		-- the VerySimpl version or the full-stats version
  | dopt Opt_D_dump_simpl_stats dflags
  = SimplCount {ticks = 0, details = emptyFM,
                n_log = 0, log1 = [], log2 = []}
  | otherwise
  = VerySimplZero

isZeroSimplCount VerySimplZero    	    = True
isZeroSimplCount (SimplCount { ticks = 0 }) = True
isZeroSimplCount other			    = False

doFreeTick tick sc@SimplCount { details = dts } 
  = dts' `seqFM` sc { details = dts' }
  where
    dts' = dts `addTick` tick 
doFreeTick tick sc = sc 

-- Gross hack to persuade GHC 3.03 to do this important seq
seqFM fm x | isEmptyFM fm = x
	   | otherwise    = x

doTick tick sc@SimplCount { ticks = tks, details = dts, n_log = nl, log1 = l1, log2 = l2 }
  | nl >= opt_HistorySize = sc1 { n_log = 1, log1 = [tick], log2 = l1 }
  | otherwise		  = sc1 { n_log = nl+1, log1 = tick : l1 }
  where
    sc1 = sc { ticks = tks+1, details = dts `addTick` tick }

doTick tick sc = VerySimplNonZero	-- The very simple case


-- Don't use plusFM_C because that's lazy, and we want to 
-- be pretty strict here!
addTick :: TickCounts -> Tick -> TickCounts
addTick fm tick = case lookupFM fm tick of
			Nothing -> addToFM fm tick 1
			Just n  -> n1 `seq` addToFM fm tick n1
				where
				   n1 = n+1


plusSimplCount sc1@(SimplCount { ticks = tks1, details = dts1 })
	       sc2@(SimplCount { ticks = tks2, details = dts2 })
  = log_base { ticks = tks1 + tks2, details = plusFM_C (+) dts1 dts2 }
  where
	-- A hackish way of getting recent log info
    log_base | null (log1 sc2) = sc1	-- Nothing at all in sc2
	     | null (log2 sc2) = sc2 { log2 = log1 sc1 }
	     | otherwise       = sc2

plusSimplCount VerySimplZero VerySimplZero = VerySimplZero
plusSimplCount sc1	     sc2	   = VerySimplNonZero

pprSimplCount VerySimplZero    = ptext SLIT("Total ticks: ZERO!")
pprSimplCount VerySimplNonZero = ptext SLIT("Total ticks: NON-ZERO!")
pprSimplCount (SimplCount { ticks = tks, details = dts, log1 = l1, log2 = l2 })
  = vcat [ptext SLIT("Total ticks:    ") <+> int tks,
	  text "",
	  pprTickCounts (fmToList dts),
	  if verboseSimplStats then
		vcat [text "",
		      ptext SLIT("Log (most recent first)"),
		      nest 4 (vcat (map ppr l1) $$ vcat (map ppr l2))]
	  else empty
    ]

pprTickCounts :: [(Tick,Int)] -> SDoc
pprTickCounts [] = empty
pprTickCounts ((tick1,n1):ticks)
  = vcat [int tot_n <+> text (tickString tick1),
	  pprTCDetails real_these,
	  pprTickCounts others
    ]
  where
    tick1_tag		= tickToTag tick1
    (these, others)	= span same_tick ticks
    real_these		= (tick1,n1):these
    same_tick (tick2,_) = tickToTag tick2 == tick1_tag
    tot_n		= sum [n | (_,n) <- real_these]

pprTCDetails ticks@((tick,_):_)
  | verboseSimplStats || isRuleFired tick
  = nest 4 (vcat [int n <+> pprTickCts tick | (tick,n) <- ticks])
  | otherwise
  = empty
\end{code}

%************************************************************************
%*									*
\subsection{Ticks}
%*									*
%************************************************************************

\begin{code}
data Tick
  = PreInlineUnconditionally	Id
  | PostInlineUnconditionally	Id

  | UnfoldingDone    		Id
  | RuleFired			FastString	-- Rule name

  | LetFloatFromLet
  | EtaExpansion		Id	-- LHS binder
  | EtaReduction		Id	-- Binder on outer lambda
  | BetaReduction		Id	-- Lambda binder


  | CaseOfCase			Id	-- Bndr on *inner* case
  | KnownBranch			Id	-- Case binder
  | CaseMerge			Id	-- Binder on outer case
  | AltMerge			Id	-- Case binder
  | CaseElim			Id	-- Case binder
  | CaseIdentity		Id	-- Case binder
  | FillInCaseDefault		Id	-- Case binder

  | BottomFound		
  | SimplifierDone		-- Ticked at each iteration of the simplifier

isRuleFired (RuleFired _) = True
isRuleFired other	  = False

instance Outputable Tick where
  ppr tick = text (tickString tick) <+> pprTickCts tick

instance Eq Tick where
  a == b = case a `cmpTick` b of { EQ -> True; other -> False }

instance Ord Tick where
  compare = cmpTick

tickToTag :: Tick -> Int
tickToTag (PreInlineUnconditionally _)	= 0
tickToTag (PostInlineUnconditionally _)	= 1
tickToTag (UnfoldingDone _)		= 2
tickToTag (RuleFired _)			= 3
tickToTag LetFloatFromLet		= 4
tickToTag (EtaExpansion _)		= 5
tickToTag (EtaReduction _)		= 6
tickToTag (BetaReduction _)		= 7
tickToTag (CaseOfCase _)		= 8
tickToTag (KnownBranch _)		= 9
tickToTag (CaseMerge _)			= 10
tickToTag (CaseElim _)			= 11
tickToTag (CaseIdentity _)		= 12
tickToTag (FillInCaseDefault _)		= 13
tickToTag BottomFound			= 14
tickToTag SimplifierDone		= 16
tickToTag (AltMerge _)			= 17

tickString :: Tick -> String
tickString (PreInlineUnconditionally _)	= "PreInlineUnconditionally"
tickString (PostInlineUnconditionally _)= "PostInlineUnconditionally"
tickString (UnfoldingDone _)		= "UnfoldingDone"
tickString (RuleFired _)		= "RuleFired"
tickString LetFloatFromLet		= "LetFloatFromLet"
tickString (EtaExpansion _)		= "EtaExpansion"
tickString (EtaReduction _)		= "EtaReduction"
tickString (BetaReduction _)		= "BetaReduction"
tickString (CaseOfCase _)		= "CaseOfCase"
tickString (KnownBranch _)		= "KnownBranch"
tickString (CaseMerge _)		= "CaseMerge"
tickString (AltMerge _)			= "AltMerge"
tickString (CaseElim _)			= "CaseElim"
tickString (CaseIdentity _)		= "CaseIdentity"
tickString (FillInCaseDefault _)	= "FillInCaseDefault"
tickString BottomFound			= "BottomFound"
tickString SimplifierDone		= "SimplifierDone"

pprTickCts :: Tick -> SDoc
pprTickCts (PreInlineUnconditionally v)	= ppr v
pprTickCts (PostInlineUnconditionally v)= ppr v
pprTickCts (UnfoldingDone v)		= ppr v
pprTickCts (RuleFired v)		= ppr v
pprTickCts LetFloatFromLet		= empty
pprTickCts (EtaExpansion v)		= ppr v
pprTickCts (EtaReduction v)		= ppr v
pprTickCts (BetaReduction v)		= ppr v
pprTickCts (CaseOfCase v)		= ppr v
pprTickCts (KnownBranch v)		= ppr v
pprTickCts (CaseMerge v)		= ppr v
pprTickCts (AltMerge v)			= ppr v
pprTickCts (CaseElim v)			= ppr v
pprTickCts (CaseIdentity v)		= ppr v
pprTickCts (FillInCaseDefault v)	= ppr v
pprTickCts other			= empty

cmpTick :: Tick -> Tick -> Ordering
cmpTick a b = case (tickToTag a `compare` tickToTag b) of
		GT -> GT
		EQ | isRuleFired a || verboseSimplStats -> cmpEqTick a b
		   | otherwise				-> EQ
		LT -> LT
	-- Always distinguish RuleFired, so that the stats
	-- can report them even in non-verbose mode

cmpEqTick :: Tick -> Tick -> Ordering
cmpEqTick (PreInlineUnconditionally a)	(PreInlineUnconditionally b)	= a `compare` b
cmpEqTick (PostInlineUnconditionally a)	(PostInlineUnconditionally b)	= a `compare` b
cmpEqTick (UnfoldingDone a)		(UnfoldingDone b)		= a `compare` b
cmpEqTick (RuleFired a)			(RuleFired b)			= a `compare` b
cmpEqTick (EtaExpansion a)		(EtaExpansion b)		= a `compare` b
cmpEqTick (EtaReduction a)		(EtaReduction b)		= a `compare` b
cmpEqTick (BetaReduction a)		(BetaReduction b)		= a `compare` b
cmpEqTick (CaseOfCase a)		(CaseOfCase b)			= a `compare` b
cmpEqTick (KnownBranch a)		(KnownBranch b)			= a `compare` b
cmpEqTick (CaseMerge a)			(CaseMerge b)			= a `compare` b
cmpEqTick (AltMerge a)			(AltMerge b)			= a `compare` b
cmpEqTick (CaseElim a)			(CaseElim b)			= a `compare` b
cmpEqTick (CaseIdentity a)		(CaseIdentity b)		= a `compare` b
cmpEqTick (FillInCaseDefault a)		(FillInCaseDefault b)		= a `compare` b
cmpEqTick other1			other2				= EQ
\end{code}



%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************


\begin{code}
data SimplEnv
  = SimplEnv {
	seMode 	    :: SimplifierMode,
	seChkr      :: SwitchChecker,
	seCC        :: CostCentreStack,	-- The enclosing CCS (when profiling)
	seSubst     :: Subst		-- The current substitution
    }
	-- The range of the substitution is OutType and OutExpr resp
	-- 
	-- The substitution is idempotent
	-- It *must* be applied; things in its domain simply aren't
	-- bound in the result.
	--
	-- The substitution usually maps an Id to its clone,
	-- but if the orig defn is a let-binding, and
	-- the RHS of the let simplifies to an atom,
	-- we just add the binding to the substitution and elide the let.

	-- The in-scope part of Subst includes *all* in-scope TyVars and Ids
	-- The elements of the set may have better IdInfo than the
	-- occurrences of in-scope Ids, and (more important) they will
	-- have a correctly-substituted type.  So we use a lookup in this
	-- set to replace occurrences

emptySimplEnv :: SimplifierMode -> [SimplifierSwitch] -> VarSet -> SimplEnv
emptySimplEnv mode switches in_scope
  = SimplEnv { seChkr = isAmongSimpl switches, seCC = subsumedCCS, seMode = mode,
	       seSubst = mkSubst (mkInScopeSet in_scope) emptySubstEnv }
	-- The top level "enclosing CC" is "SUBSUMED".

---------------------
getSwitchChecker :: SimplEnv -> SwitchChecker
getSwitchChecker env = seChkr env

---------------------
getMode :: SimplEnv -> SimplifierMode
getMode env = seMode env

setMode :: SimplifierMode -> SimplEnv -> SimplEnv
setMode mode env = env { seMode = mode }

---------------------
getEnclosingCC :: SimplEnv -> CostCentreStack
getEnclosingCC env = seCC env

setEnclosingCC :: SimplEnv -> CostCentreStack -> SimplEnv
setEnclosingCC env cc = env {seCC = cc}

---------------------
getSubst :: SimplEnv -> Subst
getSubst env = seSubst env

setSubst :: SimplEnv -> Subst -> SimplEnv
setSubst env subst = env {seSubst = subst}

extendSubst :: SimplEnv -> CoreBndr -> SubstResult -> SimplEnv
extendSubst env@(SimplEnv {seSubst = subst}) var res
  = env {seSubst = Subst.extendSubst subst var res}

extendSubstList :: SimplEnv -> [CoreBndr] -> [SubstResult] -> SimplEnv
extendSubstList env@(SimplEnv {seSubst = subst}) vars ress
  = env {seSubst = Subst.extendSubstList subst vars ress}

---------------------
getInScope :: SimplEnv -> InScopeSet
getInScope env = substInScope (seSubst env)

setInScope :: SimplEnv -> SimplEnv -> SimplEnv
setInScope env env_with_in_scope = setInScopeSet env (getInScope env_with_in_scope)

setInScopeSet :: SimplEnv -> InScopeSet -> SimplEnv
setInScopeSet env@(SimplEnv {seSubst = subst}) in_scope
  = env {seSubst = Subst.setInScope subst in_scope}

addNewInScopeIds :: SimplEnv -> [CoreBndr] -> SimplEnv
	-- The new Ids are guaranteed to be freshly allocated
addNewInScopeIds env@(SimplEnv {seSubst = subst}) vs
  = env {seSubst = Subst.extendNewInScopeList subst vs}

modifyInScope :: SimplEnv -> CoreBndr -> CoreBndr -> SimplEnv
modifyInScope env@(SimplEnv {seSubst = subst}) v v'
  = env {seSubst = Subst.modifyInScope subst v v'}

---------------------
getSubstEnv :: SimplEnv -> SubstEnv
getSubstEnv env = substEnv (seSubst env)

setSubstEnv :: SimplEnv -> SubstEnv -> SimplEnv
setSubstEnv env@(SimplEnv {seSubst = subst}) senv
  = env {seSubst = Subst.setSubstEnv subst senv}

zapSubstEnv :: SimplEnv -> SimplEnv
zapSubstEnv env@(SimplEnv {seSubst = subst})
  = env {seSubst = Subst.zapSubstEnv subst}
\end{code}


%************************************************************************
%*									*
\subsection{Decisions about inlining}
%*									*
%************************************************************************

Inlining is controlled partly by the SimplifierMode switch.  This has two
settings:

	SimplGently	(a) Simplifying before specialiser/full laziness
			(b) Simplifiying inside INLINE pragma
			(c) Simplifying the LHS of a rule
			(d) Simplifying a GHCi expression or Template 
				Haskell splice

	SimplPhase n	Used at all other times

The key thing about SimplGently is that it does no call-site inlining.
Before full laziness we must be careful not to inline wrappers,
because doing so inhibits floating
    e.g. ...(case f x of ...)...
    ==> ...(case (case x of I# x# -> fw x#) of ...)...
    ==> ...(case x of I# x# -> case fw x# of ...)...
and now the redex (f x) isn't floatable any more.

The no-inling thing is also important for Template Haskell.  You might be 
compiling in one-shot mode with -O2; but when TH compiles a splice before
running it, we don't want to use -O2.  Indeed, we don't want to inline
anything, because the byte-code interpreter might get confused about 
unboxed tuples and suchlike.

INLINE pragmas
~~~~~~~~~~~~~~
SimplGently is also used as the mode to simplify inside an InlineMe note.

\begin{code}
inlineMode :: SimplifierMode
inlineMode = SimplGently
\end{code}

It really is important to switch off inlinings inside such
expressions.  Consider the following example 

     	let f = \pq -> BIG
     	in
     	let g = \y -> f y y
	    {-# INLINE g #-}
     	in ...g...g...g...g...g...

Now, if that's the ONLY occurrence of f, it will be inlined inside g,
and thence copied multiple times when g is inlined.


This function may be inlinined in other modules, so we
don't want to remove (by inlining) calls to functions that have
specialisations, or that may have transformation rules in an importing
scope.

E.g. 	{-# INLINE f #-}
		f x = ...g...

and suppose that g is strict *and* has specialisations.  If we inline
g's wrapper, we deny f the chance of getting the specialised version
of g when f is inlined at some call site (perhaps in some other
module).

It's also important not to inline a worker back into a wrapper.
A wrapper looks like
	wraper = inline_me (\x -> ...worker... )
Normally, the inline_me prevents the worker getting inlined into
the wrapper (initially, the worker's only call site!).  But,
if the wrapper is sure to be called, the strictness analyser will
mark it 'demanded', so when the RHS is simplified, it'll get an ArgOf
continuation.  That's why the keep_inline predicate returns True for
ArgOf continuations.  It shouldn't do any harm not to dissolve the
inline-me note under these circumstances.

Note that the result is that we do very little simplification
inside an InlineMe.  

	all xs = foldr (&&) True xs
	any p = all . map p  {-# INLINE any #-}

Problem: any won't get deforested, and so if it's exported and the
importer doesn't use the inlining, (eg passes it as an arg) then we
won't get deforestation at all.  We havn't solved this problem yet!


preInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~
@preInlineUnconditionally@ examines a bndr to see if it is used just
once in a completely safe way, so that it is safe to discard the
binding inline its RHS at the (unique) usage site, REGARDLESS of how
big the RHS might be.  If this is the case we don't simplify the RHS
first, but just inline it un-simplified.

This is much better than first simplifying a perhaps-huge RHS and then
inlining and re-simplifying it.  Indeed, it can be at least quadratically
better.  Consider

	x1 = e1
	x2 = e2[x1]
	x3 = e3[x2]
	...etc...
	xN = eN[xN-1]

We may end up simplifying e1 N times, e2 N-1 times, e3 N-3 times etc.

NB: we don't even look at the RHS to see if it's trivial
We might have
			x = y
where x is used many times, but this is the unique occurrence of y.
We should NOT inline x at all its uses, because then we'd do the same
for y -- aargh!  So we must base this pre-rhs-simplification decision
solely on x's occurrences, not on its rhs.

Evne RHSs labelled InlineMe aren't caught here, because there might be
no benefit from inlining at the call site.

[Sept 01] Don't unconditionally inline a top-level thing, because that
can simply make a static thing into something built dynamically.  E.g.
	x = (a,b)
	main = \s -> h x

[Remember that we treat \s as a one-shot lambda.]  No point in
inlining x unless there is something interesting about the call site.

But watch out: if you aren't careful, some useful foldr/build fusion
can be lost (most notably in spectral/hartel/parstof) because the
foldr didn't see the build.  Doing the dynamic allocation isn't a big
deal, in fact, but losing the fusion can be.  But the right thing here
seems to be to do a callSiteInline based on the fact that there is
something interesting about the call site (it's strict).  Hmm.  That
seems a bit fragile.

Conclusion: inline top level things gaily until Phase 0 (the last
phase), at which point don't.

\begin{code}
preInlineUnconditionally :: SimplEnv -> TopLevelFlag -> InId -> Bool
preInlineUnconditionally env top_lvl bndr
  | isTopLevel top_lvl, SimplPhase 0 <- phase = False
-- If we don't have this test, consider
--	x = length [1,2,3]
-- The full laziness pass carefully floats all the cons cells to
-- top level, and preInlineUnconditionally floats them all back in.
-- Result is (a) static allocation replaced by dynamic allocation
--	     (b) many simplifier iterations because this tickles
--		 a related problem; only one inlining per pass
-- 
-- On the other hand, I have seen cases where top-level fusion is
-- lost if we don't inline top level thing (e.g. string constants)
-- Hence the test for phase zero (which is the phase for all the final
-- simplifications).  Until phase zero we take no special notice of
-- top level things, but then we become more leery about inlining
-- them.  

  | not active 		   = False
  | opt_SimplNoPreInlining = False
  | otherwise = case idOccInfo bndr of
		  IAmDead	     -> True	-- Happens in ((\x.1) v)
	  	  OneOcc in_lam once -> not in_lam && once
			-- Not inside a lambda, one occurrence ==> safe!
		  other 	     -> False
  where
    phase = getMode env
    active = case phase of
		   SimplGently  -> isAlwaysActive prag
		   SimplPhase n -> isActive n prag
    prag = idInlinePragma bndr
\end{code}

postInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~~
@postInlineUnconditionally@ decides whether to unconditionally inline
a thing based on the form of its RHS; in particular if it has a
trivial RHS.  If so, we can inline and discard the binding altogether.

NB: a loop breaker has must_keep_binding = True and non-loop-breakers
only have *forward* references Hence, it's safe to discard the binding
	
NOTE: This isn't our last opportunity to inline.  We're at the binding
site right now, and we'll get another opportunity when we get to the
ocurrence(s)

Note that we do this unconditional inlining only for trival RHSs.
Don't inline even WHNFs inside lambdas; doing so may simply increase
allocation when the function is called. This isn't the last chance; see
NOTE above.

NB: Even inline pragmas (e.g. IMustBeINLINEd) are ignored here Why?
Because we don't even want to inline them into the RHS of constructor
arguments. See NOTE above

NB: At one time even NOINLINE was ignored here: if the rhs is trivial
it's best to inline it anyway.  We often get a=E; b=a from desugaring,
with both a and b marked NOINLINE.  But that seems incompatible with
our new view that inlining is like a RULE, so I'm sticking to the 'active'
story for now.

\begin{code}
postInlineUnconditionally :: SimplEnv -> OutId -> OccInfo -> OutExpr -> Bool
postInlineUnconditionally env bndr occ_info rhs 
  =  exprIsTrivial rhs
  && active
  && not (isLoopBreaker occ_info)
  && not (isExportedId bndr)
	-- We used to have (isOneOcc occ_info) instead of
	-- not (isLoopBreaker occ_info) && not (isExportedId bndr)
	-- That was because a rather fragile use of rules got confused
	-- if you inlined even a binding f=g  e.g. We used to have
	--	map = mapList
	-- But now a more precise use of phases has eliminated this problem,
	-- so the is_active test will do the job.  I think.
	--
	-- OLD COMMENT: (delete soon)
	-- Indeed, you might suppose that
	-- there is nothing wrong with substituting for a trivial RHS, even
	-- if it occurs many times.  But consider
	--	x = y
	--	h = _inline_me_ (...x...)
	-- Here we do *not* want to have x inlined, even though the RHS is
	-- trivial, becuase the contract for an INLINE pragma is "no inlining".
	-- This is important in the rules for the Prelude 
  where
    active = case getMode env of
		   SimplGently  -> isAlwaysActive prag
		   SimplPhase n -> isActive n prag
    prag = idInlinePragma bndr

activeInline :: SimplEnv -> OutId -> OccInfo -> Bool
activeInline env id occ
  = case getMode env of
      SimplGently -> isOneOcc occ && isAlwaysActive prag
	-- No inlining at all when doing gentle stuff,
	-- except for local things that occur once
	-- The reason is that too little clean-up happens if you 
	-- don't inline use-once things.   Also a bit of inlining is *good* for
	-- full laziness; it can expose constant sub-expressions.
	-- Example in spectral/mandel/Mandel.hs, where the mandelset 
	-- function gets a useful let-float if you inline windowToViewport

	-- NB: we used to have a second exception, for data con wrappers.
	-- On the grounds that we use gentle mode for rule LHSs, and 
	-- they match better when data con wrappers are inlined.
	-- But that only really applies to the trivial wrappers (like (:)),
	-- and they are now constructed as Compulsory unfoldings (in MkId)
	-- so they'll happen anyway.

      SimplPhase n -> isActive n prag
  where
    prag = idInlinePragma id

activeRule :: SimplEnv -> Maybe (Activation -> Bool)
-- Nothing => No rules at all
activeRule env
  | opt_RulesOff = Nothing
  | otherwise
  = case getMode env of
	SimplGently  -> Just isAlwaysActive
			-- Used to be Nothing (no rules in gentle mode)
			-- Main motivation for changing is that I wanted
			-- 	lift String ===> ...
			-- to work in Template Haskell when simplifying
			-- splices, so we get simpler code for literal strings
	SimplPhase n -> Just (isActive n)
\end{code}	


%************************************************************************
%*									*
\subsubsection{Command-line switches}
%*									*
%************************************************************************

\begin{code}
getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)

switchIsOn :: (switch -> SwitchResult) -> switch -> Bool

switchIsOn lookup_fn switch
  = case (lookup_fn switch) of
      SwBool False -> False
      _	    	   -> True

intSwitchSet :: (switch -> SwitchResult)
	     -> (Int -> switch)
	     -> Maybe Int

intSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "intSwitchSet"))) of
      SwInt int -> Just int
      _	    	-> Nothing
\end{code}


\begin{code}
type SwitchChecker = SimplifierSwitch -> SwitchResult

data SwitchResult
  = SwBool	Bool		-- on/off
  | SwString	FastString	-- nothing or a String
  | SwInt	Int		-- nothing or an Int

isAmongSimpl :: [SimplifierSwitch] -> SimplifierSwitch -> SwitchResult
isAmongSimpl on_switches		-- Switches mentioned later occur *earlier*
					-- in the list; defaults right at the end.
  = let
	tidied_on_switches = foldl rm_dups [] on_switches
		-- The fold*l* ensures that we keep the latest switches;
		-- ie the ones that occur earliest in the list.

	sw_tbl :: Array Int SwitchResult
	sw_tbl = (array	(0, lAST_SIMPL_SWITCH_TAG) -- bounds...
			all_undefined)
		 // defined_elems

	all_undefined = [ (i, SwBool False) | i <- [0 .. lAST_SIMPL_SWITCH_TAG ] ]

	defined_elems = map mk_assoc_elem tidied_on_switches
    in
    -- (avoid some unboxing, bounds checking, and other horrible things:)
    case sw_tbl of { Array _ _ stuff ->
    \ switch ->
	case (indexArray# stuff (tagOf_SimplSwitch switch)) of
	  (# v #) -> v
    }
  where
    mk_assoc_elem k@(MaxSimplifierIterations lvl)
	= (iBox (tagOf_SimplSwitch k), SwInt lvl)
    mk_assoc_elem k
	= (iBox (tagOf_SimplSwitch k), SwBool True) -- I'm here, Mom!

    -- cannot have duplicates if we are going to use the array thing
    rm_dups switches_so_far switch
      = if switch `is_elem` switches_so_far
    	then switches_so_far
	else switch : switches_so_far
      where
	sw `is_elem` []     = False
	sw `is_elem` (s:ss) = (tagOf_SimplSwitch sw) ==# (tagOf_SimplSwitch s)
			    || sw `is_elem` ss
\end{code}

These things behave just like enumeration types.

\begin{code}
instance Eq SimplifierSwitch where
    a == b = tagOf_SimplSwitch a ==# tagOf_SimplSwitch b

instance Ord SimplifierSwitch where
    a <  b  = tagOf_SimplSwitch a <# tagOf_SimplSwitch b
    a <= b  = tagOf_SimplSwitch a <=# tagOf_SimplSwitch b


tagOf_SimplSwitch (MaxSimplifierIterations _)	= _ILIT(1)
tagOf_SimplSwitch NoCaseOfCase			= _ILIT(2)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

lAST_SIMPL_SWITCH_TAG = 2
\end{code}

