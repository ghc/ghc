%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplMonad (
	InId, InBind, InExpr, InAlt, InArg, InType, InBinder,
	OutId, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBinder,
	OutExprStuff, OutStuff,

	-- The continuation type
	SimplCont(..), DupFlag(..), contIsDupable, contResultType,
	contIsInteresting, pushArgs, discardCont, countValArgs, countArgs,
	contIsInline, discardInlineCont,

	-- The monad
	SimplM,
	initSmpl, returnSmpl, thenSmpl, thenSmpl_,
	mapSmpl, mapAndUnzipSmpl, mapAccumLSmpl,

	-- The inlining black-list
	getBlackList,

        -- Unique supply
        getUniqueSmpl, getUniquesSmpl,
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
	getSubst, setSubst,
	getSubstEnv, extendSubst, extendSubstList,
	getInScope, setInScope, extendInScope, extendInScopes, modifyInScope,
	setSubstEnv, zapSubstEnv,
	getSimplBinderStuff, setSimplBinderStuff,
	switchOffInlining
    ) where

#include "HsVersions.h"

import Const		( Con(DEFAULT) )
import Id		( Id, mkSysLocal, idMustBeINLINEd )
import IdInfo		( InlinePragInfo(..) )
import Demand		( Demand )
import CoreSyn
import PprCore		()	-- Instances
import Rules		( RuleBase )
import CostCentre	( CostCentreStack, subsumedCCS )
import Var		( TyVar )
import VarEnv
import VarSet
import qualified Subst
import Subst		( Subst, emptySubst, mkSubst,
			  substTy, substEnv,
			  InScopeSet, substInScope, isInScope, lookupInScope
			)
import Type             ( Type, TyVarSubst, applyTy )
import UniqSupply	( uniqsFromSupply, uniqFromSupply, splitUniqSupply,
			  UniqSupply
			)
import FiniteMap
import CmdLineOpts	( SimplifierSwitch(..), SwitchResult(..),
			  opt_PprStyle_Debug, opt_HistorySize,
			  intSwitchSet
			)
import Unique		( Unique )
import Maybes		( expectJust )
import Util		( zipWithEqual )
import Outputable

infixr 9  `thenSmpl`, `thenSmpl_`
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
\end{code}


%************************************************************************
%*									*
\subsection{The continuation data type}
%*									*
%************************************************************************

\begin{code}
type OutExprStuff = OutStuff (InScopeSet, OutExpr)
type OutStuff a   = ([OutBind], a)
	-- We return something equivalent to (let b in e), but
	-- in pieces to avoid the quadratic blowup when floating 
	-- incrementally.  Comments just before simplExprB in Simplify.lhs

data SimplCont		-- Strict contexts
  = Stop OutType		-- Type of the result

  | CoerceIt OutType			-- The To-type, simplified
	     SimplCont

  | InlinePlease			-- This continuation makes a function very
	     SimplCont			-- keen to inline itelf

  | ApplyTo  DupFlag 
	     InExpr SubstEnv		-- The argument, as yet unsimplified, 
	     SimplCont			-- and its subst-env

  | Select   DupFlag 
	     InId [InAlt] SubstEnv	-- The case binder, alts, and subst-env
	     SimplCont

  | ArgOf    DupFlag		-- An arbitrary strict context: the argument 
  	     			-- 	of a strict function, or a primitive-arg fn
				-- 	or a PrimOp
	     OutType		-- The type of the expression being sought by the context
				--	f (error "foo") ==> coerce t (error "foo")
				-- when f is strict
				-- We need to know the type t, to which to coerce.
	    (OutExpr -> SimplM OutExprStuff)	-- What to do with the result

instance Outputable SimplCont where
  ppr (Stop _)        		     = ptext SLIT("Stop")
  ppr (ApplyTo dup arg se cont)      = (ptext SLIT("ApplyTo") <+> ppr dup <+> ppr arg) $$ ppr cont
  ppr (ArgOf   dup _ _)   	     = ptext SLIT("ArgOf...") <+> ppr dup
  ppr (Select dup bndr alts se cont) = (ptext SLIT("Select") <+> ppr dup <+> ppr bndr) $$ 
				       (nest 4 (ppr alts)) $$ ppr cont
  ppr (CoerceIt ty cont)	     = (ptext SLIT("CoerceIt") <+> ppr ty) $$ ppr cont
  ppr (InlinePlease cont)	     = ptext SLIT("InlinePlease") $$ ppr cont

data DupFlag = OkToDup | NoDup

instance Outputable DupFlag where
  ppr OkToDup = ptext SLIT("ok")
  ppr NoDup   = ptext SLIT("nodup")

contIsDupable :: SimplCont -> Bool
contIsDupable (Stop _)       		 = True
contIsDupable (ApplyTo  OkToDup _ _ _)   = True
contIsDupable (ArgOf    OkToDup _ _)     = True
contIsDupable (Select   OkToDup _ _ _ _) = True
contIsDupable (CoerceIt _ cont)          = contIsDupable cont
contIsDupable (InlinePlease cont)	 = contIsDupable cont
contIsDupable other			 = False

contIsInline :: SimplCont -> Bool
contIsInline (InlinePlease cont) = True
contIsInline other		 = False

discardInlineCont :: SimplCont -> SimplCont
discardInlineCont (InlinePlease cont) = cont
discardInlineCont cont		      = cont
\end{code}


Comment about contIsInteresting
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.  

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

	let x = _coerce_ (T Int) Int (I# 3) in
	case _coerce_ Int (T Int) x of
		I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

	case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

\begin{code}
contIsInteresting :: SimplCont -> Bool
contIsInteresting (Select _ _ alts _ _)       = not (just_default alts)
contIsInteresting (CoerceIt _ cont)           = contIsInteresting cont
contIsInteresting (ApplyTo _ (Type _) _ cont) = contIsInteresting cont
contIsInteresting (ApplyTo _ _	      _ _)    = True
contIsInteresting (ArgOf _ _ _)		      = True
	-- If this call is the arg of a strict function, the context
	-- is a bit interesting.  If we inline here, we may get useful
	-- evaluation information to avoid repeated evals: e.g.
	--	x + (y * z)
	-- Here the contIsInteresting makes the '*' keener to inline,
	-- which in turn exposes a constructor which makes the '+' inline.
	-- Assuming that +,* aren't small enough to inline regardless.
contIsInteresting (InlinePlease _)	      = True
contIsInteresting other		              = False

just_default [(DEFAULT,_,_)] = True	-- See notes below for why we look
just_default alts	     = False	-- for this special case
\end{code}


\begin{code}
pushArgs :: SubstEnv -> [InExpr] -> SimplCont -> SimplCont
pushArgs se []         cont = cont
pushArgs se (arg:args) cont = ApplyTo NoDup arg se (pushArgs se args cont)

discardCont :: SimplCont	-- A continuation, expecting
	    -> SimplCont	-- Replace the continuation with a suitable coerce
discardCont (Stop to_ty) = Stop to_ty
discardCont cont	 = CoerceIt to_ty (Stop to_ty)
			 where
			   to_ty = contResultType cont

contResultType :: SimplCont -> OutType
contResultType (Stop to_ty)	     = to_ty
contResultType (ArgOf _ to_ty _)     = to_ty
contResultType (ApplyTo _ _ _ cont)  = contResultType cont
contResultType (CoerceIt _ cont)     = contResultType cont
contResultType (InlinePlease cont)   = contResultType cont
contResultType (Select _ _ _ _ cont) = contResultType cont

countValArgs :: SimplCont -> Int
countValArgs (ApplyTo _ (Type ty) se cont) = countValArgs cont
countValArgs (ApplyTo _ val_arg   se cont) = 1 + countValArgs cont
countValArgs other			   = 0

countArgs :: SimplCont -> Int
countArgs (ApplyTo _ arg se cont) = 1 + countArgs cont
countArgs other			  = 0
\end{code}


%************************************************************************
%*									*
\subsection{Monad plumbing}
%*									*
%************************************************************************

For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)

\begin{code}
type SimplM result		-- We thread the unique supply because
  =  SimplEnv			-- constantly splitting it is rather expensive
  -> UniqSupply
  -> SimplCount 
  -> (result, UniqSupply, SimplCount)

data SimplEnv
  = SimplEnv {
	seChkr      :: SwitchChecker,
	seCC        :: CostCentreStack,	-- The enclosing CCS (when profiling)
	seBlackList :: Id -> Bool,	-- True =>  don't inline this Id
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
initSmpl :: SwitchChecker
	 -> UniqSupply		-- No init count; set to 0
	 -> VarSet		-- In scope (usually empty, but useful for nested calls)
	 -> (Id -> Bool)	-- Black-list function
	 -> SimplM a
	 -> (a, SimplCount)

initSmpl chkr us in_scope black_list m
  = case m (emptySimplEnv chkr in_scope black_list) us zeroSimplCount of 
	(result, _, count) -> (result, count)


{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}

returnSmpl :: a -> SimplM a
returnSmpl e env us sc = (e, us, sc)

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k env us0 sc0
  = case (m env us0 sc0) of 
	(m_result, us1, sc1) -> k m_result env us1 sc1

thenSmpl_ m k env us0 sc0
  = case (m env us0 sc0) of 
	(_, us1, sc1) -> k env us1 sc1
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
getUniqueSmpl :: SimplM Unique
getUniqueSmpl env us sc = case splitUniqSupply us of
				(us1, us2) -> (uniqFromSupply us1, us2, sc)

getUniquesSmpl :: Int -> SimplM [Unique]
getUniquesSmpl n env us sc = case splitUniqSupply us of
				(us1, us2) -> (uniqsFromSupply n us1, us2, sc)
\end{code}


%************************************************************************
%*									*
\subsection{Counting up what we've done}
%*									*
%************************************************************************

\begin{code}
getSimplCount :: SimplM SimplCount
getSimplCount env us sc = (sc, us, sc)

tick :: Tick -> SimplM ()
tick t env us sc = sc' `seq` ((), us, sc')
		 where
		   sc' = doTick t sc

freeTick :: Tick -> SimplM ()
-- Record a tick, but don't add to the total tick count, which is
-- used to decide when nothing further has happened
freeTick t env us sc = sc' `seq` ((), us, sc')
		 where
		   sc' = doFreeTick t sc
\end{code}

\begin{code}
verboseSimplStats = opt_PprStyle_Debug		-- For now, anyway

-- Defined both with and without debugging
zeroSimplCount	   :: SimplCount
isZeroSimplCount   :: SimplCount -> Bool
pprSimplCount	   :: SimplCount -> SDoc
doTick, doFreeTick :: Tick -> SimplCount -> SimplCount
plusSimplCount     :: SimplCount -> SimplCount -> SimplCount
\end{code}

\begin{code}
#ifndef DEBUG
----------------------------------------------------------
--			Debugging OFF
----------------------------------------------------------
type SimplCount = Int

zeroSimplCount = 0

isZeroSimplCount n = n==0

doTick     t n = n+1	-- Very basic when not debugging
doFreeTick t n = n	-- Don't count leaf visits

pprSimplCount n = ptext SLIT("Total ticks:") <+> int n

plusSimplCount n m = n+m

#else
----------------------------------------------------------
--			Debugging ON
----------------------------------------------------------

data SimplCount = SimplCount	{
			ticks   :: !Int,		-- Total ticks
			details :: !TickCounts,		-- How many of each type
			n_log	:: !Int,		-- N
			log1	:: [Tick],		-- Last N events; <= opt_HistorySize
			log2	:: [Tick]		-- Last opt_HistorySize events before that
		  }

type TickCounts = FiniteMap Tick Int

zeroSimplCount = SimplCount {ticks = 0, details = emptyFM,
			     n_log = 0, log1 = [], log2 = []}

isZeroSimplCount sc = ticks sc == 0

doFreeTick tick sc@SimplCount { details = dts } 
  = dts' `seqFM` sc { details = dts' }
  where
    dts' = dts `addTick` tick 

-- Gross hack to persuade GHC 3.03 to do this important seq
seqFM fm x | isEmptyFM fm = x
	   | otherwise    = x

doTick tick sc@SimplCount { ticks = tks, details = dts, n_log = nl, log1 = l1, log2 = l2 }
  | nl >= opt_HistorySize = sc1 { n_log = 1, log1 = [tick], log2 = l1 }
  | otherwise		  = sc1 { n_log = nl+1, log1 = tick : l1 }
  where
    sc1 = sc { ticks = tks+1, details = dts `addTick` tick }

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
#endif
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

  | LetFloatFromLet		Id	-- Thing floated out
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
  | LeafVisit
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
tickToTag (LetFloatFromLet _)		= 4
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
tickToTag LeafVisit			= 15
tickToTag SimplifierDone		= 16

tickString :: Tick -> String
tickString (PreInlineUnconditionally _)	= "PreInlineUnconditionally"
tickString (PostInlineUnconditionally _)= "PostInlineUnconditionally"
tickString (UnfoldingDone _)		= "UnfoldingDone"
tickString (RuleFired _)		= "RuleFired"
tickString (LetFloatFromLet _)		= "LetFloatFromLet"
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
tickString LeafVisit			= "LeafVisit"

pprTickCts :: Tick -> SDoc
pprTickCts (PreInlineUnconditionally v)	= ppr v
pprTickCts (PostInlineUnconditionally v)= ppr v
pprTickCts (UnfoldingDone v)		= ppr v
pprTickCts (RuleFired v)		= ppr v
pprTickCts (LetFloatFromLet v)		= ppr v
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
cmpEqTick (LetFloatFromLet a)		(LetFloatFromLet b)		= a `compare` b
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
getSwitchChecker env us sc = (seChkr env, us, sc)

getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)
\end{code}


@switchOffInlining@ is used to prepare the environment for simplifying
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

We prepare the envt by simply modifying the in_scope_env, which has all the
unfolding info. At one point we did it by modifying the chkr so that
it said "EssentialUnfoldingsOnly", but that prevented legitmate, and
important, simplifications happening in the body of the RHS.

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

ALSO NOTE that we do all this by modifing the inline-pragma,
not by zapping the unfolding.  The latter may still be useful for
knowing when something is evaluated.

June 98 update: I've gone back to dealing with this by adding
the EssentialUnfoldingsOnly switch.  That doesn't stop essential
unfoldings, nor inlineUnconditionally stuff; and the thing's going
to be inlined at every call site anyway.  Running over the whole
environment seems like wild overkill.

\begin{code}
switchOffInlining :: SimplM a -> SimplM a
switchOffInlining m env us sc
  = m (env { seBlackList = \v -> True  }) us sc
\end{code}


%************************************************************************
%*									*
\subsubsection{The ``enclosing cost-centre''}
%*									*
%************************************************************************

\begin{code}
getEnclosingCC :: SimplM CostCentreStack
getEnclosingCC env us sc = (seCC env, us, sc)

setEnclosingCC :: CostCentreStack -> SimplM a -> SimplM a
setEnclosingCC cc m env us sc = m (env { seCC = cc }) us sc
\end{code}


%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************


\begin{code}
emptySimplEnv :: SwitchChecker -> InScopeSet -> (Id -> Bool) -> SimplEnv

emptySimplEnv sw_chkr in_scope black_list
  = SimplEnv { seChkr = sw_chkr, seCC = subsumedCCS,
	       seBlackList = black_list,
	       seSubst = mkSubst in_scope emptySubstEnv }
	-- The top level "enclosing CC" is "SUBSUMED".

getSubst :: SimplM Subst
getSubst env us sc = (seSubst env, us, sc)

getBlackList :: SimplM (Id -> Bool)
getBlackList env us sc = (seBlackList env, us, sc)

setSubst :: Subst -> SimplM a -> SimplM a
setSubst subst m env us sc = m (env {seSubst = subst}) us sc

getSubstEnv :: SimplM SubstEnv
getSubstEnv env us sc = (substEnv (seSubst env), us, sc)

extendInScope :: CoreBndr -> SimplM a -> SimplM a
extendInScope v m env@(SimplEnv {seSubst = subst}) us sc
  = m (env {seSubst = Subst.extendInScope subst v}) us sc

extendInScopes :: [CoreBndr] -> SimplM a -> SimplM a
extendInScopes vs m env@(SimplEnv {seSubst = subst}) us sc
  = m (env {seSubst = Subst.extendInScopes subst vs}) us sc

getInScope :: SimplM InScopeSet
getInScope env us sc = (substInScope (seSubst env), us, sc)

setInScope :: InScopeSet -> SimplM a -> SimplM a
setInScope in_scope m env@(SimplEnv {seSubst = subst}) us sc
  = m (env {seSubst = Subst.setInScope subst in_scope}) us sc

modifyInScope :: CoreBndr -> SimplM a -> SimplM a
modifyInScope v m env us sc 
#ifdef DEBUG
  | not (v `isInScope` seSubst env)
  = pprTrace "modifyInScope: not in scope:" (ppr v)
    m env us sc
#endif
  | otherwise
  = extendInScope v m env us sc

extendSubst :: CoreBndr -> SubstResult -> SimplM a -> SimplM a
extendSubst var res m env@(SimplEnv {seSubst = subst}) us sc
  = m (env { seSubst = Subst.extendSubst subst var res  }) us sc

extendSubstList :: [CoreBndr] -> [SubstResult] -> SimplM a -> SimplM a
extendSubstList vars ress m env@(SimplEnv {seSubst = subst}) us sc
  = m (env { seSubst = Subst.extendSubstList subst vars ress  }) us sc

setSubstEnv :: SubstEnv -> SimplM a -> SimplM a
setSubstEnv senv m env@(SimplEnv {seSubst = subst}) us sc
  = m (env {seSubst = Subst.setSubstEnv subst senv}) us sc

zapSubstEnv :: SimplM a -> SimplM a
zapSubstEnv m env@(SimplEnv {seSubst = subst}) us sc
  = m (env {seSubst = Subst.zapSubstEnv subst}) us sc

getSimplBinderStuff :: SimplM (Subst, UniqSupply)
getSimplBinderStuff (SimplEnv {seSubst = subst}) us sc
  = ((subst, us), us, sc)

setSimplBinderStuff :: (Subst, UniqSupply) -> SimplM a -> SimplM a
setSimplBinderStuff (subst, us) m env _ sc
  = m (env {seSubst = subst}) us sc
\end{code}


\begin{code}
newId :: Type -> (Id -> SimplM a) -> SimplM a
	-- Extends the in-scope-env too
newId ty m env@(SimplEnv {seSubst = subst}) us sc
  =  case splitUniqSupply us of
	(us1, us2) -> m v (env {seSubst = Subst.extendInScope subst v}) us2 sc
		   where
		      v = mkSysLocal SLIT("s") (uniqFromSupply us1) ty

newIds :: [Type] -> ([Id] -> SimplM a) -> SimplM a
newIds tys m env@(SimplEnv {seSubst = subst}) us sc
  =  case splitUniqSupply us of
	(us1, us2) -> m vs (env {seSubst = Subst.extendInScopes subst vs}) us2 sc
		   where
		      vs = zipWithEqual "newIds" (mkSysLocal SLIT("s")) 
					(uniqsFromSupply (length tys) us1) tys

\end{code}
