%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[StrictAnal]{``Simple'' Mycroft-style strictness analyser}

The original version(s) of all strictness-analyser code (except the
Semantique analyser) was written by Andy Gill.

\begin{code}
#include "HsVersions.h"

module StrictAnal ( saWwTopBinds, saTopBinds ) where

IMPORT_Trace
import Outputable
import Pretty

import CmdLineOpts	( GlobalSwitch(..) )
import CoreSyn		-- ToDo: get pprCoreBinding straight from PlainCore?
import Id		( addIdDemandInfo, isWrapperId, addIdStrictness,
			  getIdUniType, getIdDemandInfo
			  IF_ATTACK_PRAGMAS(COMMA getIdStrictness) -- profiling
			)
import IdEnv
import IdInfo
import PlainCore
import SaAbsInt
import SaLib
import SplitUniq
import Unique
import Util
import WorkWrap		-- "back-end" of strictness analyser
import WwLib		( WwM(..) )
\end{code}


%************************************************************************
%*									*
\subsection[Thoughts]{Random thoughts}
%*									*
%************************************************************************

A note about worker-wrappering.  If we have

	f :: Int -> Int
	f = let v = <expensive>
	    in \x -> <body>

and we deduce that f is strict, it is nevertheless NOT safe to worker-wapper to

	f = \x -> case x of Int x# -> fw x#
	fw = \x# -> let x = Int x# 
		    in 
		    let v = <expensive>
		    in <body>

because this obviously loses laziness, since now <expensive> 
is done each time.  Alas.

WATCH OUT!  This can mean that something is unboxed only to be
boxed again. For example

	g x y = f x

Here g is strict, and *will* split into worker-wrapper.  A call to
g, with the wrapper inlined will then be

	case arg of Int a# -> gw a#

Now g calls f, which has no wrapper, so it has to box it.

	gw = \a# -> f (Int a#)

Alas and alack.


%************************************************************************
%*									*
\subsection[iface-StrictAnal]{Interface to the outside world}
%*									*
%************************************************************************

\begin{code}
saWwTopBinds :: SplitUniqSupply
	     -> (GlobalSwitch -> Bool)
	     -> [PlainCoreBinding]
	     -> [PlainCoreBinding]

saWwTopBinds us switch_chker binds
  = let
	strflags = (switch_chker AllStrict, switch_chker NumbersStrict)

	-- mark each binder with its strictness
#ifndef OMIT_STRANAL_STATS
	(binds_w_strictness, sa_stats)
	  = sa_top_binds strflags binds nullSaStats
#else
	binds_w_strictness
	  = sa_top_binds strflags binds
#endif
    in
    -- possibly show what we decided about strictness...
    (if switch_chker D_dump_stranal
     then pprTrace "Strictness:\n" (ppAboves (
	   map (pprCoreBinding PprDebug pprBigCoreBinder pprBigCoreBinder ppr) binds_w_strictness))
     else id
    )
    -- possibly show how many things we marked as demanded...
    ((if switch_chker D_simplifier_stats
#ifndef OMIT_STRANAL_STATS
     then pp_stats sa_stats
#else
     then id
#endif
     else id
    )
	-- create worker/wrappers, and mark binders with their
	-- "strictness info" [which encodes their
	-- worker/wrapper-ness]
    (workersAndWrappers binds_w_strictness us switch_chker))
#ifndef OMIT_STRANAL_STATS
  where
    pp_stats (SaStats tlam dlam tc dc tlet dlet)
      = pprTrace "Binders marked demanded: "
	(ppBesides [ppStr "Lambda vars: ", ppInt IBOX(dlam), ppChar '/', ppInt IBOX(tlam),
		  ppStr "; Case vars: ",   ppInt IBOX(dc),   ppChar '/', ppInt IBOX(tc),
		  ppStr "; Let vars: ",    ppInt IBOX(dlet), ppChar '/', ppInt IBOX(tlet)
	])
#endif
\end{code}

%************************************************************************
%*									*
\subsection[saBinds]{Strictness analysis of bindings}
%*									*
%************************************************************************

[Some of the documentation about types, etc., in \tr{SaLib} may be
helpful for understanding this module.]

@saTopBinds@ tags each binder in the program with its @Demand@.
That tells how each binder is {\em used}; if @Strict@, then the binder
is sure to be evaluated to HNF; if @NonStrict@ it may or may not be;
if @Absent@, then it certainly is not used. [DATED; ToDo: update]

(The above info is actually recorded for posterity in each binder's
IdInfo, notably its @DemandInfo@.)

We proceed by analysing the bindings top-to-bottom, building up an
environment which maps @Id@s to their abstract values (i.e., an
@AbsValEnv@ maps an @Id@ to its @AbsVal@).

\begin{code}
saTopBinds   :: StrAnalFlags -> [PlainCoreBinding] -> [PlainCoreBinding]     -- exported
sa_top_binds :: StrAnalFlags -> [PlainCoreBinding] -> SaM [PlainCoreBinding] -- not exported

saTopBinds strflags binds
#ifndef OMIT_STRANAL_STATS
  = fst (sa_top_binds strflags binds nullSaStats)
#else
  = sa_top_binds strflags binds
#endif

sa_top_binds strflags binds
  = let
	starting_abs_env = nullAbsValEnv strflags
    in
    do_it starting_abs_env starting_abs_env binds
  where
    do_it _    _    [] = returnSa []
    do_it senv aenv (b:bs)
      = saTopBind senv  aenv  b  `thenSa` \ (senv2, aenv2, new_b) ->
	do_it     senv2 aenv2 bs `thenSa` \ new_bs ->
	returnSa (new_b : new_bs)
\end{code}

@saTopBind@ is only used for the top level.  We don't add any demand
info to these ids because we can't work it out.  In any case, it
doesn't do us any good to know whether top-level binders are sure to
be used; we can't turn top-level @let@s into @case@s.

\begin{code}
saTopBind :: StrictEnv -> AbsenceEnv
	  -> PlainCoreBinding
	  -> SaM (StrictEnv, AbsenceEnv, PlainCoreBinding)

saTopBind str_env abs_env (CoNonRec binder rhs)
  = saExpr str_env abs_env rhs 	`thenSa` \ new_rhs ->
    let
	strflags = getStrAnalFlags str_env

	str_rhs = absEval StrAnal rhs str_env
	abs_rhs = absEval AbsAnal rhs abs_env

	widened_str_rhs = widen StrAnal str_rhs
	widened_abs_rhs = widen AbsAnal abs_rhs
		-- The widening above is done for efficiency reasons.
		-- See notes on CoLet case in SaAbsInt.lhs

	new_binder
	  = addStrictnessInfoToId
		strflags
		widened_str_rhs widened_abs_rhs
		binder
		rhs

	  -- Augment environments with a mapping of the
	  -- binder to its abstract values, computed by absEval
      	new_str_env = addOneToAbsValEnv str_env binder widened_str_rhs
      	new_abs_env = addOneToAbsValEnv abs_env binder widened_abs_rhs
    in
    returnSa (new_str_env, new_abs_env, CoNonRec new_binder new_rhs)

saTopBind str_env abs_env (CoRec pairs)
  = let
	strflags    = getStrAnalFlags str_env
	(binders,rhss) = unzip pairs
	str_rhss    = fixpoint StrAnal binders rhss str_env
	abs_rhss    = fixpoint AbsAnal binders rhss abs_env
		      -- fixpoint returns widened values
      	new_str_env = growAbsValEnvList str_env (binders `zip` str_rhss)
      	new_abs_env = growAbsValEnvList abs_env (binders `zip` abs_rhss)
	new_binders = zipWith4 (addStrictnessInfoToId strflags)
				str_rhss abs_rhss binders rhss
    in
    mapSa (saExpr new_str_env new_abs_env) rhss	`thenSa` \ new_rhss ->
    let
	new_pairs   = new_binders `zip` new_rhss
    in
    returnSa (new_str_env, new_abs_env, CoRec new_pairs)
\end{code}

%************************************************************************
%*									*
\subsection[saExpr]{Strictness analysis of an expression}
%*									*
%************************************************************************

@saExpr@ computes the strictness of an expression within a given
environment.

\begin{code}
saExpr :: StrictEnv -> AbsenceEnv -> PlainCoreExpr -> SaM PlainCoreExpr

saExpr _ _ e@(CoVar _)      = returnSa e
saExpr _ _ e@(CoLit _)      = returnSa e
saExpr _ _ e@(CoCon _ _ _)  = returnSa e
saExpr _ _ e@(CoPrim _ _ _) = returnSa e

saExpr str_env abs_env (CoLam args body)
  = saExpr str_env abs_env body	`thenSa` \ new_body ->
    let
      	new_args  = addDemandInfoToIds str_env abs_env body args
    in
    tickLambdas new_args	`thenSa_` -- stats
    returnSa (CoLam new_args new_body)

saExpr str_env abs_env (CoTyLam ty expr)
  = saExpr str_env abs_env expr	`thenSa` \ new_expr ->
    returnSa (CoTyLam ty new_expr)

saExpr str_env abs_env (CoApp fun arg)
  = saExpr str_env abs_env fun	`thenSa` \ new_fun ->
    returnSa (CoApp new_fun arg)

saExpr str_env abs_env (CoTyApp expr ty)
  = saExpr str_env abs_env expr	`thenSa` \ new_expr ->
    returnSa (CoTyApp new_expr ty)

saExpr str_env abs_env (CoSCC cc expr)
  = saExpr str_env abs_env expr	`thenSa` \ new_expr ->
    returnSa (CoSCC cc new_expr)

saExpr str_env abs_env (CoCase expr (CoAlgAlts alts deflt))
  = saExpr    str_env abs_env expr  `thenSa` \ new_expr  ->
    saDefault str_env abs_env deflt `thenSa` \ new_deflt ->
    mapSa sa_alt alts		    `thenSa` \ new_alts  ->
    returnSa (CoCase new_expr (CoAlgAlts new_alts new_deflt))
  where
    sa_alt (con, binders, rhs)
      = saExpr str_env abs_env rhs  `thenSa` \ new_rhs ->
	let
	    new_binders = addDemandInfoToIds str_env abs_env rhs binders
	in
	tickCases new_binders	    `thenSa_` -- stats
	returnSa (con, new_binders, new_rhs)

saExpr str_env abs_env (CoCase expr (CoPrimAlts alts deflt))
  = saExpr    str_env abs_env expr  `thenSa` \ new_expr  ->
    saDefault str_env abs_env deflt `thenSa` \ new_deflt ->
    mapSa sa_alt alts		    `thenSa` \ new_alts  ->
    returnSa (CoCase new_expr (CoPrimAlts new_alts new_deflt))
  where
    sa_alt (lit, rhs)
      = saExpr str_env abs_env rhs `thenSa` \ new_rhs ->
	returnSa (lit, new_rhs)

saExpr str_env abs_env (CoLet (CoNonRec binder rhs) body)
  =	-- Analyse the RHS in the environment at hand
    saExpr str_env abs_env rhs  `thenSa` \ new_rhs  ->
    let
	strflags = getStrAnalFlags str_env

	-- Bind this binder to the abstract value of the RHS; analyse
	-- the body of the `let' in the extended environment.
      	str_rhs_val  	= absEval StrAnal rhs str_env
      	abs_rhs_val  	= absEval AbsAnal rhs abs_env

	widened_str_rhs = widen StrAnal str_rhs_val
	widened_abs_rhs = widen AbsAnal abs_rhs_val
		-- The widening above is done for efficiency reasons.
		-- See notes on CoLet case in SaAbsInt.lhs

      	new_str_env	= addOneToAbsValEnv str_env binder widened_str_rhs
      	new_abs_env	= addOneToAbsValEnv abs_env binder widened_abs_rhs

	-- Now determine the strictness of this binder; use that info
	-- to record DemandInfo/StrictnessInfo in the binder.
	new_binder = addStrictnessInfoToId strflags
			widened_str_rhs widened_abs_rhs
			(addDemandInfoToId str_env abs_env body binder)
			rhs
    in
    tickLet new_binder			`thenSa_` -- stats
    saExpr new_str_env new_abs_env body	`thenSa` \ new_body ->
    returnSa (CoLet (CoNonRec new_binder new_rhs) new_body)

saExpr str_env abs_env (CoLet (CoRec pairs) body)
  = let
	strflags       = getStrAnalFlags str_env
	(binders,rhss) = unzip pairs
	str_vals       = fixpoint StrAnal binders rhss str_env
	abs_vals       = fixpoint AbsAnal binders rhss abs_env
			 -- fixpoint returns widened values
      	new_str_env    = growAbsValEnvList str_env (binders `zip` str_vals)
      	new_abs_env    = growAbsValEnvList abs_env (binders `zip` abs_vals)
    in
    saExpr new_str_env new_abs_env body		`thenSa` \ new_body ->
    mapSa (saExpr new_str_env new_abs_env) rhss	`thenSa` \ new_rhss ->
    let
--	new_binders      = addDemandInfoToIds new_str_env new_abs_env body binders
-- 		DON'T add demand info in a CoRec!
--		a) it's useless: we can't do let-to-case
--		b) it's incorrect.  Consider
--			letrec x = ...y...
--			       y = ...x...
--			in ...x...
--		   When we ask whether y is demanded we'll bind y to bottom and
--		   evaluate the body of the letrec.  But that will result in our
--		   deciding that y is absent, which is plain wrong!
--		It's much easier simply not to do this.

	improved_binders = zipWith4 (addStrictnessInfoToId strflags)
				    str_vals abs_vals binders rhss

	whiter_than_white_binders = launder improved_binders

      	new_pairs   = whiter_than_white_binders `zip` new_rhss
    in
    returnSa (CoLet (CoRec new_pairs) new_body)
  where
    launder me = {-still-} me
\end{code}

\begin{code}
saDefault str_env abs_env CoNoDefault = returnSa CoNoDefault

saDefault str_env abs_env (CoBindDefault bdr rhs)
  = saExpr str_env abs_env rhs	`thenSa` \ new_rhs ->
    let
	new_bdr = addDemandInfoToId str_env abs_env rhs bdr
    in
    tickCases [new_bdr]		`thenSa_` -- stats
    returnSa (CoBindDefault new_bdr new_rhs)
\end{code}


%************************************************************************
%*									*
\subsection[computeInfos]{Add computed info to binders}
%*									*
%************************************************************************

Important note (Sept 93).  @addStrictnessInfoToId@ is used only for
let(rec) bound variables, and is use to attach the strictness (not
demand) info to the binder.  We are careful to restrict this
strictness info to the lambda-bound arguments which are actually
visible, at the top level, lest we accidentally lose laziness by
eagerly looking for an "extra" argument.  So we "dig for lambdas" in a
rather syntactic way.

A better idea might be to have some kind of arity analysis to
tell how many args could safely be grabbed.

\begin{code}
addStrictnessInfoToId 
	:: StrAnalFlags
	-> AbsVal 		-- Abstract strictness value
	-> AbsVal		-- Ditto absence
	-> Id 			-- The id
	-> PlainCoreExpr	-- Its RHS
	-> Id			-- Augmented with strictness

addStrictnessInfoToId strflags str_val abs_val binder body
  = if isWrapperId binder then
	binder	-- Avoid clobbering existing strictness info 
		-- (and, more importantly, worker info).
		-- Deeply suspicious (SLPJ)
    else
    if (isBot str_val) then
	binder `addIdStrictness` mkBottomStrictnessInfo
    else
	case (digForLambdas body) of { (_, lambda_bounds, rhs) ->
        let
		tys        = map getIdUniType lambda_bounds
		strictness = findStrictness strflags tys str_val abs_val
	in
	binder `addIdStrictness` mkStrictnessInfo strictness Nothing
	}
\end{code}

\begin{code}
addDemandInfoToId :: StrictEnv -> AbsenceEnv 
		  -> PlainCoreExpr 	-- The scope of the id
		  -> Id 
		  -> Id			-- Id augmented with Demand info

addDemandInfoToId str_env abs_env expr binder
  = binder `addIdDemandInfo` (mkDemandInfo (findDemand str_env abs_env expr binder))

addDemandInfoToIds :: StrictEnv -> AbsenceEnv -> PlainCoreExpr -> [Id] -> [Id]

addDemandInfoToIds str_env abs_env expr binders 
  = map (addDemandInfoToId str_env abs_env expr) binders
\end{code}

%************************************************************************
%*									*
\subsection{Monad used herein for stats}
%*									*
%************************************************************************

\begin{code}
data SaStats
  = SaStats FAST_INT FAST_INT	-- total/marked-demanded lambda-bound
	    FAST_INT FAST_INT	-- total/marked-demanded case-bound
	    FAST_INT FAST_INT	-- total/marked-demanded let-bound
				-- (excl. top-level; excl. letrecs)

nullSaStats = SaStats ILIT(0) ILIT(0) ILIT(0) ILIT(0) ILIT(0) ILIT(0)

thenSa	      :: SaM a -> (a -> SaM b) -> SaM b
thenSa_	      :: SaM a -> SaM b -> SaM b
returnSa      :: a -> SaM a

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenSa #-}
{-# INLINE thenSa_ #-}
{-# INLINE returnSa #-}
#endif

tickLambdas :: [Id] -> SaM ()
tickCases   :: [Id] -> SaM ()
tickLet     :: Id   -> SaM ()

#ifndef OMIT_STRANAL_STATS
type SaM a = SaStats -> (a, SaStats)

thenSa expr cont stats
  = case (expr stats) of { (result, stats1) ->
    cont result stats1 }

thenSa_ expr cont stats
  = case (expr stats) of { (_, stats1) ->
    cont stats1 }

returnSa x stats = (x, stats)

tickLambdas vars (SaStats tlam dlam tc dc tlet dlet)
  = case (foldr tick_demanded (0,0) vars) of { (IBOX(tot), IBOX(demanded)) ->
    ((), SaStats (tlam _ADD_ tot) (dlam _ADD_ demanded) tc dc tlet dlet) }

tickCases vars (SaStats tlam dlam tc dc tlet dlet)
  = case (foldr tick_demanded (0,0) vars) of { (IBOX(tot), IBOX(demanded)) ->
    ((), SaStats tlam dlam (tc _ADD_ tot) (dc _ADD_ demanded) tlet dlet) }

tickLet var (SaStats tlam dlam tc dc tlet dlet)
  = case (tick_demanded var (0,0))        of { (IBOX(tot),IBOX(demanded)) ->
    ((), SaStats tlam dlam tc dc (tlet _ADD_ tot) (dlet _ADD_ demanded)) }

tick_demanded var (tot, demanded)
  = (tot + 1,
     if (willBeDemanded (getIdDemandInfo var))
     then demanded + 1
     else demanded)

#else {-OMIT_STRANAL_STATS-}
-- identity monad
type SaM a = a

thenSa expr cont = cont expr

thenSa_ expr cont = cont

returnSa x = x

tickLambdas vars = panic "OMIT_STRANAL_STATS: tickLambdas"
tickCases   vars = panic "OMIT_STRANAL_STATS: tickCases"
tickLet     var  = panic "OMIT_STRANAL_STATS: tickLet"

#endif {-OMIT_STRANAL_STATS-}

mapSa	      :: (a -> SaM b) -> [a] -> SaM [b]

mapSa f []     = returnSa []
mapSa f (x:xs)
  = f x		`thenSa` \ r  ->
    mapSa f xs	`thenSa` \ rs ->
    returnSa (r:rs)
\end{code}
