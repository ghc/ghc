%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplMonad (
	InId, InBind, InExpr, InAlt, InArg, InType, InBinder,
	OutId, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBinder,
	OutExprStuff, OutStuff, returnOutStuff,

	-- The monad
	SimplM,
	initSmpl, returnSmpl, thenSmpl, thenSmpl_,
	mapSmpl, mapAndUnzipSmpl, mapAccumLSmpl,
	getDOptsSmpl,

	-- The inlining black-list
	setBlackList, getBlackList, noInlineBlackList,

        -- Unique supply
        getUniqueSmpl, getUniquesSmpl, getUniqSupplySmpl,
	newId, newIds,

	-- Counting
	SimplCount, Tick(..),
	tick, freeTick,
	getSimplCount, zeroSimplCount, pprSimplCount, 
	plusSimplCount, isZeroSimplCount,

	-- Switch checker
	SwitchChecker, getSwitchChecker, getSimplIntSwitch,

	-- Cost centres
	getEnclosingCC, setEnclosingCC,

	-- Environments
	getEnv, setAllExceptInScope,
	getSubst, setSubst,
	getSubstEnv, extendSubst, extendSubstList,
	getInScope, setInScope, modifyInScope, addNewInScopeIds,
	setSubstEnv, zapSubstEnv,
	getSimplBinderStuff, setSimplBinderStuff,

	-- Adding bindings
	addLetBind, addLetBinds, addAuxiliaryBind, addAuxiliaryBinds,
	addCaseBind, needsCaseBinding, addNonRecBind, wrapFloats, addFloats
    ) where

#include "HsVersions.h"

import Id		( Id, mkSysLocal, idType, idUnfolding, isDataConWrapId,
			  isGlobalId )
import CoreSyn
import CoreUnfold	( isCompulsoryUnfolding )
import CoreUtils	( exprOkForSpeculation )
import PprCore		()	-- Instances
import CostCentre	( CostCentreStack, subsumedCCS )
import OccName		( UserFS )
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
import CmdLineOpts	( SimplifierSwitch(..), SwitchResult(..),
			  DynFlags, DynFlag(..), dopt,
			  opt_PprStyle_Debug, opt_HistorySize,
			  intSwitchSet
			)
import Unique		( Unique )
import Maybes		( expectJust )
import Util		( zipWithEqual )
import Outputable

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
type OutType	= Type			-- Cloned
type OutBind	= CoreBind
type OutExpr	= CoreExpr
type OutAlt	= CoreAlt
type OutArg	= CoreArg

type SwitchChecker = SimplifierSwitch -> SwitchResult

type OutExprStuff = OutStuff OutExpr
type OutStuff a   = (OrdList OutBind, (InScopeSet, a))
	-- We return something equivalent to (let b in e), but
	-- in pieces to avoid the quadratic blowup when floating 
	-- incrementally.  Comments just before simplExprB in Simplify.lhs
\end{code}

\begin{code}
wrapFloats :: OrdList CoreBind -> CoreExpr -> CoreExpr
wrapFloats binds body = foldOL Let body binds

returnOutStuff :: a -> SimplM (OutStuff a)
returnOutStuff x = getInScope 	`thenSmpl` \ in_scope ->
		   returnSmpl (nilOL, (in_scope, x))

addFloats :: OrdList CoreBind -> InScopeSet -> SimplM (OutStuff a) -> SimplM (OutStuff a)
addFloats floats in_scope thing_inside
  = setInScope in_scope thing_inside	`thenSmpl` \ (binds, res) ->
    returnSmpl (floats `appOL` binds, res)
 
addLetBind :: CoreBind -> SimplM (OutStuff a) -> SimplM (OutStuff a)
addLetBind bind thing_inside
  = thing_inside	`thenSmpl` \ (binds, res) ->
    returnSmpl (bind `consOL` binds, res)

addLetBinds :: [CoreBind] -> SimplM (OutStuff a) -> SimplM (OutStuff a)
addLetBinds binds1 thing_inside
  = thing_inside	`thenSmpl` \ (binds2, res) ->
    returnSmpl (toOL binds1 `appOL` binds2, res)

addAuxiliaryBinds :: [CoreBind] -> SimplM (OutStuff a) -> SimplM (OutStuff a)
	-- Extends the in-scope environment as well as wrapping the bindings
addAuxiliaryBinds binds1 thing_inside
  = addNewInScopeIds (bindersOfBinds binds1)	$
    addLetBinds binds1 thing_inside

addAuxiliaryBind :: CoreBind -> SimplM (OutStuff a) -> SimplM (OutStuff a)
	-- Extends the in-scope environment as well as wrapping the bindings
addAuxiliaryBind bind thing_inside
  = addNewInScopeIds (bindersOf bind)	$
    addLetBind bind thing_inside

needsCaseBinding ty rhs = isUnLiftedType ty && not (exprOkForSpeculation rhs)
	-- Make a case expression instead of a let
	-- These can arise either from the desugarer,
	-- or from beta reductions: (\x.e) (x +# y)

addCaseBind bndr rhs thing_inside
  = thing_inside		`thenSmpl` \ (floats, (_, body)) ->
    returnOutStuff (Case rhs bndr [(DEFAULT, [], wrapFloats floats body)])

addNonRecBind bndr rhs thing_inside
	-- Checks for needing a case binding
  | needsCaseBinding (idType bndr) rhs = addCaseBind bndr rhs thing_inside
  | otherwise		     	       = addLetBind  (NonRec bndr rhs) thing_inside
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
  =  DynFlags
  -> SimplEnv		-- We thread the unique supply because
  -> UniqSupply		-- constantly splitting it is rather expensive
  -> SimplCount 
  -> (result, UniqSupply, SimplCount)

type BlackList = Id -> Bool	-- True =>  don't inline this Id

data SimplEnv
  = SimplEnv {
	seChkr      :: SwitchChecker,
	seCC        :: CostCentreStack,	-- The enclosing CCS (when profiling)
	seBlackList :: BlackList,
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
\end{code}

\begin{code}
initSmpl :: DynFlags
	 -> SwitchChecker
	 -> UniqSupply		-- No init count; set to 0
	 -> VarSet		-- In scope (usually empty, but useful for nested calls)
	 -> BlackList		-- Black-list function
	 -> SimplM a
	 -> (a, SimplCount)

initSmpl dflags chkr us in_scope black_list m
  = case m dflags (emptySimplEnv chkr in_scope black_list) us 
	   (zeroSimplCount dflags) of 
	(result, _, count) -> (result, count)


{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}

returnSmpl :: a -> SimplM a
returnSmpl e dflags env us sc = (e, us, sc)

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k dflags env us0 sc0
  = case (m dflags env us0 sc0) of 
	(m_result, us1, sc1) -> k m_result dflags env us1 sc1

thenSmpl_ m k dflags env us0 sc0
  = case (m dflags env us0 sc0) of 
	(_, us1, sc1) -> k dflags env us1 sc1
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
getUniqSupplySmpl dflags env us sc 
   = case splitUniqSupply us of
        (us1, us2) -> (us1, us2, sc)

getUniqueSmpl :: SimplM Unique
getUniqueSmpl dflags env us sc 
   = case splitUniqSupply us of
        (us1, us2) -> (uniqFromSupply us1, us2, sc)

getUniquesSmpl :: SimplM [Unique]
getUniquesSmpl dflags env us sc 
   = case splitUniqSupply us of
        (us1, us2) -> (uniqsFromSupply us1, us2, sc)

getDOptsSmpl :: SimplM DynFlags
getDOptsSmpl dflags env us sc 
   = (dflags, us, sc)
\end{code}


%************************************************************************
%*									*
\subsection{Counting up what we've done}
%*									*
%************************************************************************

\begin{code}
getSimplCount :: SimplM SimplCount
getSimplCount dflags env us sc = (sc, us, sc)

tick :: Tick -> SimplM ()
tick t dflags env us sc 
   = sc' `seq` ((), us, sc')
     where
        sc' = doTick t sc

freeTick :: Tick -> SimplM ()
-- Record a tick, but don't add to the total tick count, which is
-- used to decide when nothing further has happened
freeTick t dflags env us sc 
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
  | RuleFired			FAST_STRING	-- Rule name

  | LetFloatFromLet
  | EtaExpansion		Id	-- LHS binder
  | EtaReduction		Id	-- Binder on outer lambda
  | BetaReduction		Id	-- Lambda binder


  | CaseOfCase			Id	-- Bndr on *inner* case
  | KnownBranch			Id	-- Case binder
  | CaseMerge			Id	-- Binder on outer case
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
cmpEqTick (CaseElim a)			(CaseElim b)			= a `compare` b
cmpEqTick (CaseIdentity a)		(CaseIdentity b)		= a `compare` b
cmpEqTick (FillInCaseDefault a)		(FillInCaseDefault b)		= a `compare` b
cmpEqTick other1			other2				= EQ
\end{code}


%************************************************************************
%*									*
\subsubsection{Command-line switches}
%*									*
%************************************************************************

\begin{code}
getSwitchChecker :: SimplM SwitchChecker
getSwitchChecker dflags env us sc = (seChkr env, us, sc)

getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)
\end{code}


@setBlackList@ is used to prepare the environment for simplifying
the RHS of an Id that's marked with an INLINE pragma.  It is going to
be inlined wherever they are used, and then all the inlining will take
effect.  Meanwhile, there isn't much point in doing anything to the
as-yet-un-INLINEd rhs.  Furthremore, it's very important to switch off
inlining!  because
	(a) not doing so will inline a worker straight back into its wrapper!

and 	(b) Consider the following example 
	     	let f = \pq -> BIG
	     	in
	     	let g = \y -> f y y
		    {-# INLINE g #-}
	     	in ...g...g...g...g...g...

	Now, if that's the ONLY occurrence of f, it will be inlined inside g,
	and thence copied multiple times when g is inlined.

	Andy disagrees! Example:
		all xs = foldr (&&) True xs
		any p = all . map p  {-# INLINE any #-}
	
	Problem: any won't get deforested, and so if it's exported and
	the importer doesn't use the inlining, (eg passes it as an arg)
	then we won't get deforestation at all.
	We havn't solved this problem yet!

We prepare the envt by simply modifying the black list.

6/98 update: 

We *don't* prevent inlining from happening for identifiers
that are marked as IMustBeINLINEd. An example of where
doing this is crucial is:
  
   class Bar a => Foo a where
     ...g....
   {-# INLINE f #-}
   f :: Foo a => a -> b
   f x = ....Foo_sc1...
   
If `f' needs to peer inside Foo's superclass, Bar, it refers
to the appropriate super class selector, which is marked as
must-inlineable. We don't generate any code for a superclass
selector, so failing to inline it in the RHS of `f' will
leave a reference to a non-existent id, with bad consequences.

ALSO NOTE that we do all this by modifing the black list
not by zapping the unfolding.  The latter may still be useful for
knowing when something is evaluated.

\begin{code}
setBlackList :: BlackList -> SimplM a -> SimplM a
setBlackList black_list m dflags env us sc 
   = m dflags (env { seBlackList = black_list }) us sc

getBlackList :: SimplM BlackList
getBlackList dflags env us sc = (seBlackList env, us, sc)

noInlineBlackList :: SimplM BlackList
	-- Inside inlinings, black list anything that is in scope or imported.
	-- except for data con wrappers.  The exception is a hack, like the one in
	-- SimplCore.simplRules, to make wrappers inline in rule LHSs.
	-- We may as well do the same here.
noInlineBlackList dflags env us sc = (blacklisted,us,sc)
	where blacklisted v =
	   	  not (isDataConWrapId v) &&
		  (v `isInScope` (seSubst env) || isGlobalId v)
	-- NB: An earlier version omitted the last clause; this meant 
	-- that even inlinings *completely within* an INLINE didn't happen. 
	-- This was cheaper, and probably adequate, but produced awful code
        -- for some dictionary constructions.
\end{code}


%************************************************************************
%*									*
\subsubsection{The ``enclosing cost-centre''}
%*									*
%************************************************************************

\begin{code}
getEnclosingCC :: SimplM CostCentreStack
getEnclosingCC dflags env us sc = (seCC env, us, sc)

setEnclosingCC :: CostCentreStack -> SimplM a -> SimplM a
setEnclosingCC cc m dflags env us sc = m dflags (env { seCC = cc }) us sc
\end{code}


%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************


\begin{code}
emptySimplEnv :: SwitchChecker -> VarSet -> (Id -> Bool) -> SimplEnv

emptySimplEnv sw_chkr in_scope black_list
  = SimplEnv { seChkr = sw_chkr, seCC = subsumedCCS,
	       seBlackList = black_list,
	       seSubst = mkSubst (mkInScopeSet in_scope) emptySubstEnv }
	-- The top level "enclosing CC" is "SUBSUMED".

getEnv :: SimplM SimplEnv
getEnv dflags env us sc = (env, us, sc)

setAllExceptInScope :: SimplEnv -> SimplM a -> SimplM a
setAllExceptInScope new_env@(SimplEnv {seSubst = new_subst}) m dflags
		    	    (SimplEnv {seSubst = old_subst}) us sc 
  = m dflags (new_env {seSubst = Subst.setInScope new_subst (substInScope old_subst)}) 
             us sc

getSubst :: SimplM Subst
getSubst dflags env us sc = (seSubst env, us, sc)

setSubst :: Subst -> SimplM a -> SimplM a
setSubst subst m dflags env us sc = m dflags (env {seSubst = subst}) us sc

getSubstEnv :: SimplM SubstEnv
getSubstEnv dflags env us sc = (substEnv (seSubst env), us, sc)

addNewInScopeIds :: [CoreBndr] -> SimplM a -> SimplM a
	-- The new Ids are guaranteed to be freshly allocated
addNewInScopeIds vs m dflags env@(SimplEnv {seSubst = subst}) us sc
  = m dflags (env {seSubst = Subst.extendNewInScopeList subst vs}) us sc

getInScope :: SimplM InScopeSet
getInScope dflags env us sc = (substInScope (seSubst env), us, sc)

setInScope :: InScopeSet -> SimplM a -> SimplM a
setInScope in_scope m dflags env@(SimplEnv {seSubst = subst}) us sc
  = m dflags (env {seSubst = Subst.setInScope subst in_scope}) us sc

modifyInScope :: CoreBndr -> CoreBndr -> SimplM a -> SimplM a
modifyInScope v v' m dflags env@(SimplEnv {seSubst = subst}) us sc 
  = m dflags (env {seSubst = Subst.modifyInScope subst v v'}) us sc

extendSubst :: CoreBndr -> SubstResult -> SimplM a -> SimplM a
extendSubst var res m dflags env@(SimplEnv {seSubst = subst}) us sc
  = m dflags (env { seSubst = Subst.extendSubst subst var res  }) us sc

extendSubstList :: [CoreBndr] -> [SubstResult] -> SimplM a -> SimplM a
extendSubstList vars ress m dflags env@(SimplEnv {seSubst = subst}) us sc
  = m dflags (env { seSubst = Subst.extendSubstList subst vars ress  }) us sc

setSubstEnv :: SubstEnv -> SimplM a -> SimplM a
setSubstEnv senv m dflags env@(SimplEnv {seSubst = subst}) us sc
  = m dflags (env {seSubst = Subst.setSubstEnv subst senv}) us sc

zapSubstEnv :: SimplM a -> SimplM a
zapSubstEnv m dflags env@(SimplEnv {seSubst = subst}) us sc
  = m dflags (env {seSubst = Subst.zapSubstEnv subst}) us sc

getSimplBinderStuff :: SimplM (Subst, UniqSupply)
getSimplBinderStuff dflags (SimplEnv {seSubst = subst}) us sc
  = ((subst, us), us, sc)

setSimplBinderStuff :: (Subst, UniqSupply) -> SimplM a -> SimplM a
setSimplBinderStuff (subst, us) m dflags env _ sc
  = m dflags (env {seSubst = subst}) us sc
\end{code}


\begin{code}
newId :: UserFS -> Type -> (Id -> SimplM a) -> SimplM a
	-- Extends the in-scope-env too
newId fs ty m dflags env@(SimplEnv {seSubst = subst}) us sc
  =  case splitUniqSupply us of
	(us1, us2) -> m v dflags (env {seSubst = Subst.extendNewInScope subst v}) 
			us2 sc
		   where
		      v = mkSysLocal fs (uniqFromSupply us1) ty

newIds :: UserFS -> [Type] -> ([Id] -> SimplM a) -> SimplM a
newIds fs tys m dflags env@(SimplEnv {seSubst = subst}) us sc
  =  case splitUniqSupply us of
	(us1, us2) -> m vs dflags (env {seSubst = Subst.extendNewInScopeList subst vs}) 
			us2 sc
		   where
		      vs = zipWith (mkSysLocal fs) (uniqsFromSupply us1) tys
\end{code}
