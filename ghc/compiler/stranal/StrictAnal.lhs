%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[StrictAnal]{``Simple'' Mycroft-style strictness analyser}

The original version(s) of all strictness-analyser code (except the
Semantique analyser) was written by Andy Gill.

\begin{code}
module StrictAnal ( saBinds ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_stranal, opt_D_dump_simpl_stats,  opt_D_verbose_core2core )
import CoreSyn
import Id		( idType, setIdStrictness,
			  getIdDemandInfo, setIdDemandInfo,
			  Id
			)
import IdInfo		( mkStrictnessInfo )
import CoreLint		( beginPass, endPass )
import ErrUtils		( dumpIfSet )
import SaAbsInt
import SaLib
import Demand		( isStrict )
import UniqSupply       ( UniqSupply )
import Util		( zipWith4Equal )
import Outputable
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

@saBinds@ decorates bindings with strictness info.  A later 
worker-wrapper pass can use this info to create wrappers and
strict workers.

\begin{code}
saBinds ::[CoreBind]
	   -> IO [CoreBind]

saBinds binds
  = do {
	beginPass "Strictness analysis";

	-- Mark each binder with its strictness
#ifndef OMIT_STRANAL_STATS
	let { (binds_w_strictness, sa_stats) = saTopBinds binds nullSaStats };
	dumpIfSet opt_D_dump_simpl_stats "Strictness analysis statistics"
		  (pp_stats sa_stats);
#else
	let { binds_w_strictness = saTopBindsBinds binds };
#endif

	endPass "Strictness analysis" (opt_D_dump_stranal || opt_D_verbose_core2core) binds_w_strictness
    }
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
saTopBinds :: [CoreBind] -> SaM [CoreBind] -- not exported

saTopBinds binds
  = let
	starting_abs_env = nullAbsValEnv
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
	  -> CoreBind
	  -> SaM (StrictEnv, AbsenceEnv, CoreBind)

saTopBind str_env abs_env (NonRec binder rhs)
  = saExpr str_env abs_env rhs 	`thenSa` \ new_rhs ->
    let
	str_rhs = absEval StrAnal rhs str_env
	abs_rhs = absEval AbsAnal rhs abs_env

	widened_str_rhs = widen StrAnal str_rhs
	widened_abs_rhs = widen AbsAnal abs_rhs
		-- The widening above is done for efficiency reasons.
		-- See notes on Let case in SaAbsInt.lhs

	new_binder
	  = addStrictnessInfoToId
		widened_str_rhs widened_abs_rhs
		binder
		rhs

	  -- Augment environments with a mapping of the
	  -- binder to its abstract values, computed by absEval
      	new_str_env = addOneToAbsValEnv str_env binder widened_str_rhs
      	new_abs_env = addOneToAbsValEnv abs_env binder widened_abs_rhs
    in
    returnSa (new_str_env, new_abs_env, NonRec new_binder new_rhs)

saTopBind str_env abs_env (Rec pairs)
  = let
	(binders,rhss) = unzip pairs
	str_rhss    = fixpoint StrAnal binders rhss str_env
	abs_rhss    = fixpoint AbsAnal binders rhss abs_env
		      -- fixpoint returns widened values
      	new_str_env = growAbsValEnvList str_env (binders `zip` str_rhss)
      	new_abs_env = growAbsValEnvList abs_env (binders `zip` abs_rhss)
	new_binders = zipWith4Equal "saTopBind" addStrictnessInfoToId
				    str_rhss abs_rhss binders rhss
    in
    mapSa (saExpr new_str_env new_abs_env) rhss	`thenSa` \ new_rhss ->
    let
	new_pairs   = new_binders `zip` new_rhss
    in
    returnSa (new_str_env, new_abs_env, Rec new_pairs)
\end{code}

%************************************************************************
%*									*
\subsection[saExpr]{Strictness analysis of an expression}
%*									*
%************************************************************************

@saExpr@ computes the strictness of an expression within a given
environment.

\begin{code}
saExpr :: StrictEnv -> AbsenceEnv -> CoreExpr -> SaM CoreExpr

saExpr _ _ e@(Var _)	= returnSa e
saExpr _ _ e@(Con  _ _)	= returnSa e
saExpr _ _ e@(Type _)	= returnSa e

saExpr str_env abs_env (Lam bndr body)
  =	-- Don't bother to set the demand-info on a lambda binder
	-- We do that only for let(rec)-bound functions
    saExpr str_env abs_env body	`thenSa` \ new_body ->
    returnSa (Lam bndr new_body)

saExpr str_env abs_env (App fun arg)
  = saExpr str_env abs_env fun	`thenSa` \ new_fun ->
    saExpr str_env abs_env arg	`thenSa` \ new_arg ->
    returnSa (App new_fun new_arg)

saExpr str_env abs_env (Note note expr)
  = saExpr str_env abs_env expr	`thenSa` \ new_expr ->
    returnSa (Note note new_expr)

saExpr str_env abs_env (Case expr case_bndr alts)
  = saExpr str_env abs_env expr		`thenSa` \ new_expr  ->
    mapSa sa_alt alts			`thenSa` \ new_alts  ->
    let
	new_case_bndr = addDemandInfoToCaseBndr str_env abs_env alts case_bndr
    in
    returnSa (Case new_expr new_case_bndr new_alts)
  where
    sa_alt (con, binders, rhs)
      = saExpr str_env abs_env rhs  `thenSa` \ new_rhs ->
	let
	    new_binders = map add_demand_info binders
	    add_demand_info bndr | isTyVar bndr = bndr
			  	 | otherwise	= addDemandInfoToId str_env abs_env rhs bndr
	in
	tickCases new_binders	    `thenSa_` -- stats
	returnSa (con, new_binders, new_rhs)

saExpr str_env abs_env (Let (NonRec binder rhs) body)
  =	-- Analyse the RHS in the environment at hand
    saExpr str_env abs_env rhs  `thenSa` \ new_rhs  ->
    let
	-- Bind this binder to the abstract value of the RHS; analyse
	-- the body of the `let' in the extended environment.
      	str_rhs_val  	= absEval StrAnal rhs str_env
      	abs_rhs_val  	= absEval AbsAnal rhs abs_env

	widened_str_rhs = widen StrAnal str_rhs_val
	widened_abs_rhs = widen AbsAnal abs_rhs_val
		-- The widening above is done for efficiency reasons.
		-- See notes on Let case in SaAbsInt.lhs

      	new_str_env	= addOneToAbsValEnv str_env binder widened_str_rhs
      	new_abs_env	= addOneToAbsValEnv abs_env binder widened_abs_rhs

	-- Now determine the strictness of this binder; use that info
	-- to record DemandInfo/StrictnessInfo in the binder.
	new_binder = addStrictnessInfoToId
			widened_str_rhs widened_abs_rhs
			(addDemandInfoToId str_env abs_env body binder)
			rhs
    in
    tickLet new_binder			`thenSa_` -- stats
    saExpr new_str_env new_abs_env body	`thenSa` \ new_body ->
    returnSa (Let (NonRec new_binder new_rhs) new_body)

saExpr str_env abs_env (Let (Rec pairs) body)
  = let
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
-- 		DON'T add demand info in a Rec!
--		a) it's useless: we can't do let-to-case
--		b) it's incorrect.  Consider
--			letrec x = ...y...
--			       y = ...x...
--			in ...x...
--		   When we ask whether y is demanded we'll bind y to bottom and
--		   evaluate the body of the letrec.  But that will result in our
--		   deciding that y is absent, which is plain wrong!
--		It's much easier simply not to do this.

	improved_binders = zipWith4Equal "saExpr" addStrictnessInfoToId
				         str_vals abs_vals binders rhss

      	new_pairs   = improved_binders `zip` new_rhss
    in
    returnSa (Let (Rec new_pairs) new_body)
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
	:: AbsVal 		-- Abstract strictness value
	-> AbsVal		-- Ditto absence
	-> Id 			-- The id
	-> CoreExpr	-- Its RHS
	-> Id			-- Augmented with strictness

addStrictnessInfoToId str_val abs_val binder body
  = case collectBinders body of
	-- We could use 'collectBindersIgnoringNotes', but then the 
	-- strictness info may have more items than the visible binders
	-- used by WorkWrap.tryWW
	(binders, rhs) -> -- pprTrace "addStr" (ppr binder $$ ppr strictness) $
			  binder `setIdStrictness` 
			  mkStrictnessInfo strictness
		where
		    tys        = [idType id | id <- binders, isId id]
		    strictness = findStrictness tys str_val abs_val
\end{code}

\begin{code}
addDemandInfoToId :: StrictEnv -> AbsenceEnv
		  -> CoreExpr 	-- The scope of the id
		  -> Id
		  -> Id			-- Id augmented with Demand info

addDemandInfoToId str_env abs_env expr binder
  = binder `setIdDemandInfo` (findDemand str_env abs_env expr binder)

addDemandInfoToCaseBndr str_env abs_env alts binder
  = binder `setIdDemandInfo` (findDemandAlts str_env abs_env alts binder)

addDemandInfoToIds :: StrictEnv -> AbsenceEnv -> CoreExpr -> [Id] -> [Id]

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

{-# INLINE thenSa #-}
{-# INLINE thenSa_ #-}
{-# INLINE returnSa #-}

tickLambda :: Id   -> SaM ()
tickCases  :: [CoreBndr] -> SaM ()
tickLet    :: Id   -> SaM ()

#ifndef OMIT_STRANAL_STATS
type SaM a = SaStats -> (a, SaStats)

thenSa expr cont stats
  = case (expr stats) of { (result, stats1) ->
    cont result stats1 }

thenSa_ expr cont stats
  = case (expr stats) of { (_, stats1) ->
    cont stats1 }

returnSa x stats = (x, stats)

tickLambda var (SaStats tlam dlam tc dc tlet dlet)
  = case (tick_demanded var (0,0)) of { (IBOX(tot), IBOX(demanded)) ->
    ((), SaStats (tlam _ADD_ tot) (dlam _ADD_ demanded) tc dc tlet dlet) }

tickCases vars (SaStats tlam dlam tc dc tlet dlet)
  = case (foldr tick_demanded (0,0) vars) of { (IBOX(tot), IBOX(demanded)) ->
    ((), SaStats tlam dlam (tc _ADD_ tot) (dc _ADD_ demanded) tlet dlet) }

tickLet var (SaStats tlam dlam tc dc tlet dlet)
  = case (tick_demanded var (0,0))        of { (IBOX(tot),IBOX(demanded)) ->
    ((), SaStats tlam dlam tc dc (tlet _ADD_ tot) (dlet _ADD_ demanded)) }

tick_demanded var (tot, demanded)
  | isTyVar var = (tot, demanded)
  | otherwise
  = (tot + 1,
     if (isStrict (getIdDemandInfo var))
     then demanded + 1
     else demanded)

pp_stats (SaStats tlam dlam tc dc tlet dlet)
      = hcat [ptext SLIT("Lambda vars: "), int IBOX(dlam), char '/', int IBOX(tlam),
		    ptext SLIT("; Case vars: "), int IBOX(dc),   char '/', int IBOX(tc),
		    ptext SLIT("; Let vars: "),  int IBOX(dlet), char '/', int IBOX(tlet)
	]

#else {-OMIT_STRANAL_STATS-}
-- identity monad
type SaM a = a

thenSa expr cont = cont expr

thenSa_ expr cont = cont

returnSa x = x

tickLambda var  = panic "OMIT_STRANAL_STATS: tickLambda"
tickCases  vars = panic "OMIT_STRANAL_STATS: tickCases"
tickLet    var  = panic "OMIT_STRANAL_STATS: tickLet"

#endif {-OMIT_STRANAL_STATS-}

mapSa	      :: (a -> SaM b) -> [a] -> SaM [b]

mapSa f []     = returnSa []
mapSa f (x:xs)
  = f x		`thenSa` \ r  ->
    mapSa f xs	`thenSa` \ rs ->
    returnSa (r:rs)
\end{code}
