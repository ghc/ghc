%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}

\begin{code}
module WorkWrap ( wwTopBinds, mkWrapper ) where

#include "HsVersions.h"

import CoreSyn
import CoreUnfold	( Unfolding, certainlySmallEnoughToInline, calcUnfoldingGuidance )
import CmdLineOpts	( opt_UF_CreationThreshold , opt_D_verbose_core2core, 
                          opt_D_dump_worker_wrapper
			)
import CoreLint		( beginPass, endPass )
import CoreUtils	( coreExprType, exprEtaExpandArity )
import Const		( Con(..) )
import DataCon		( DataCon )
import MkId		( mkWorkerId )
import Id		( Id, idType, getIdStrictness, setIdArity, isOneShotLambda,
			  setIdStrictness, getIdDemandInfo, getInlinePragma,
			  setIdWorkerInfo, getIdCprInfo )
import VarSet
import Type		( Type, isNewType, splitForAllTys, splitFunTys )
import IdInfo		( mkStrictnessInfo, noStrictnessInfo, StrictnessInfo(..),
			  CprInfo(..), exactArity, InlinePragInfo(..)
			)
import Demand           ( Demand, wwLazy )
import SaLib
import UniqSupply	( UniqSupply, initUs_, returnUs, thenUs, mapUs, getUniqueUs, UniqSM )
import UniqSet
import WwLib
import Outputable
\end{code}

We take Core bindings whose binders have:

\begin{enumerate}

\item Strictness attached (by the front-end of the strictness
analyser), and / or

\item Constructed Product Result information attached by the CPR
analysis pass.

\end{enumerate}

and we return some ``plain'' bindings which have been
worker/wrapper-ified, meaning: 

\begin{enumerate} 

\item Functions have been split into workers and wrappers where
appropriate.  If a function has both strictness and CPR properties
then only one worker/wrapper doing both transformations is produced;

\item Binders' @IdInfos@ have been updated to reflect the existence of
these workers/wrappers (this is where we get STRICTNESS and CPR pragma
info for exported values).
\end{enumerate}

\begin{code}

wwTopBinds :: UniqSupply
	     -> [CoreBind]
	     -> IO [CoreBind]

wwTopBinds us binds
  = do {
	beginPass "Worker Wrapper binds";

	-- Create worker/wrappers, and mark binders with their
	-- "strictness info" [which encodes their worker/wrapper-ness]
	let { binds' = workersAndWrappers us binds };

	endPass "Worker Wrapper binds" (opt_D_dump_worker_wrapper || 
                                        opt_D_verbose_core2core) binds'
    }
\end{code}


\begin{code}
workersAndWrappers :: UniqSupply -> [CoreBind] -> [CoreBind]

workersAndWrappers us top_binds
  = initUs_ us $
    mapUs wwBind top_binds `thenUs` \ top_binds' ->
    returnUs (concat top_binds')
\end{code}

%************************************************************************
%*									*
\subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}
%*									*
%************************************************************************

@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...

\begin{code}
wwBind	:: CoreBind
	-> UniqSM [CoreBind]	-- returns a WwBinding intermediate form;
				-- the caller will convert to Expr/Binding,
				-- as appropriate.

wwBind (NonRec binder rhs)
  = wwExpr rhs						`thenUs` \ new_rhs ->
    tryWW True {- non-recursive -} binder new_rhs 	`thenUs` \ new_pairs ->
    returnUs [NonRec b e | (b,e) <- new_pairs]
      -- Generated bindings must be non-recursive
      -- because the original binding was.

------------------------------

wwBind (Rec pairs)
  = mapUs do_one pairs		`thenUs` \ new_pairs ->
    returnUs [Rec (concat new_pairs)]
  where
    do_one (binder, rhs) = wwExpr rhs 	`thenUs` \ new_rhs ->
			   tryWW False {- recursive -} binder new_rhs
\end{code}

@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.
???????????????? ToDo

\begin{code}
wwExpr :: CoreExpr -> UniqSM CoreExpr

wwExpr e@(Type _)   = returnUs e
wwExpr e@(Var _)    = returnUs e

wwExpr e@(Con con args)
 = mapUs wwExpr args	`thenUs` \ args' ->
   returnUs (Con con args')

wwExpr (Lam binder expr)
  = wwExpr expr			`thenUs` \ new_expr ->
    returnUs (Lam binder new_expr)

wwExpr (App f a)
  = wwExpr f			`thenUs` \ new_f ->
    wwExpr a			`thenUs` \ new_a ->
    returnUs (App new_f new_a)

wwExpr (Note note expr)
  = wwExpr expr			`thenUs` \ new_expr ->
    returnUs (Note note new_expr)

wwExpr (Let bind expr)
  = wwBind bind			`thenUs` \ intermediate_bind ->
    wwExpr expr			`thenUs` \ new_expr ->
    returnUs (mkLets intermediate_bind new_expr)

wwExpr (Case expr binder alts)
  = wwExpr expr				`thenUs` \ new_expr ->
    mapUs ww_alt alts			`thenUs` \ new_alts ->
    returnUs (Case new_expr binder new_alts)
  where
    ww_alt (con, binders, rhs)
      =	wwExpr rhs			`thenUs` \ new_rhs ->
	returnUs (con, binders, new_rhs)
\end{code}

%************************************************************************
%*									*
\subsection[tryWW]{@tryWW@: attempt a worker/wrapper pair}
%*									*
%************************************************************************

@tryWW@ just accumulates arguments, converts strictness info from the
front-end into the proper form, then calls @mkWwBodies@ to do
the business.

We have to BE CAREFUL that we don't worker-wrapperize an Id that has
already been w-w'd!  (You can end up with several liked-named Ids
bouncing around at the same time---absolute mischief.)  So the
criterion we use is: if an Id already has an unfolding (for whatever
reason), then we don't w-w it.

The only reason this is monadised is for the unique supply.

\begin{code}
tryWW	:: Bool				-- True <=> a non-recursive binding
	-> Id				-- The fn binder
	-> CoreExpr			-- The bound rhs; its innards
					--   are already ww'd
	-> UniqSM [(Id, CoreExpr)]	-- either *one* or *two* pairs;
					-- if one, then no worker (only
					-- the orig "wrapper" lives on);
					-- if two, then a worker and a
					-- wrapper.
tryWW non_rec fn_id rhs
  | (non_rec &&		-- Don't split if its non-recursive and small
     certainlySmallEnoughToInline (calcUnfoldingGuidance opt_UF_CreationThreshold rhs)
	-- No point in worker/wrappering something that is going to be
	-- INLINEd wholesale anyway.  If the strictness analyser is run
	-- twice, this test also prevents wrappers (which are INLINEd)
	-- from being re-done.
    )

  || arity == 0		-- Don't split if it's not a function
  || never_inline fn_id

  || not (do_strict_ww || do_cpr_ww || do_coerce_ww)
  = returnUs [ (fn_id, rhs) ]

  | otherwise		-- Do w/w split
  = mkWwBodies fun_ty arity wrap_dmds one_shots cpr_info	`thenUs` \ (work_args, wrap_fn, work_fn) ->
    getUniqueUs							`thenUs` \ work_uniq ->
    let
	work_rhs     = work_fn rhs
	work_demands = [getIdDemandInfo v | v <- work_args, isId v]
	proto_work_id   	 = mkWorkerId work_uniq fn_id (coreExprType work_rhs) 
	work_id | has_strictness = proto_work_id `setIdStrictness` mkStrictnessInfo (work_demands, result_bot)
		| otherwise	 = proto_work_id

	wrap_rhs = wrap_fn work_id
	wrap_id  = fn_id `setIdStrictness`      wrapper_strictness
                         `setIdWorkerInfo`	Just work_id
			 `setIdArity`		exactArity arity
		-- Add info to the wrapper:
		--	(a) we want to set its arity
		-- 	(b) we want to pin on its revised strictness info
		--	(c) we pin on its worker id 
    in
    returnUs ([(work_id, work_rhs), (wrap_id, wrap_rhs)])
	-- Worker first, because wrapper mentions it
  where
    fun_ty = idType fn_id
    arity  = exprEtaExpandArity rhs

	-- Don't split something which is marked unconditionally NOINLINE
    never_inline fn_id = case getInlinePragma fn_id of
				IMustNotBeINLINEd False Nothing -> True
				other			        -> False

    strictness_info     		  = getIdStrictness fn_id
    StrictnessInfo arg_demands result_bot = strictness_info
    has_strictness			  = case strictness_info of
						StrictnessInfo _ _ -> True
						other		   -> False
			
    do_strict_ww = has_strictness && worthSplitting wrap_dmds result_bot

	-- NB: There maybe be more items in arg_demands than arity, because
	-- the strictness info is semantic and looks through InlineMe and Scc Notes, 
	-- whereas arity does not
    demands_for_visible_args = take arity arg_demands
    remaining_arg_demands    = drop arity arg_demands

    wrap_dmds | has_strictness = setUnpackStrategy demands_for_visible_args
	      | otherwise      = take arity (repeat wwLazy)

    wrapper_strictness | has_strictness = mkStrictnessInfo (wrap_dmds ++ remaining_arg_demands, result_bot)
	               | otherwise      = noStrictnessInfo

	-------------------------------------------------------------
    cpr_info  = getIdCprInfo fn_id
    do_cpr_ww = case cpr_info of
			CPRInfo _ -> True
			other	  -> False

	-------------------------------------------------------------
    do_coerce_ww = check_for_coerce arity fun_ty

	-------------------------------------------------------------
    one_shots = get_one_shots rhs

-- See if there's a Coerce before we run out of arity;
-- if so, it's worth trying a w/w split.  Reason: we find
-- functions like	f = coerce (\s -> e)
--	     and	g = \x -> coerce (\s -> e)
-- and they may have no useful strictness or cpr info, but if we
-- do the w/w thing we get rid of the coerces.  

check_for_coerce arity ty
  = length arg_tys <= arity && isNewType res_ty
	-- Don't look further than arity args, 
	-- but if there are arity or fewer, see if there's
	-- a newtype in the corner
  where
    (_, tau) 	      = splitForAllTys ty
    (arg_tys, res_ty) = splitFunTys tau

-- If the original function has one-shot arguments, it is important to
-- make the wrapper and worker have corresponding one-shot arguments too.
-- Otherwise we spuriously float stuff out of case-expression join points,
-- which is very annoying.
get_one_shots (Lam b e)
  | isId b    = isOneShotLambda b : get_one_shots e
  | otherwise = get_one_shots e
get_one_shots (Note _ e) = get_one_shots e
get_one_shots other	 = noOneShotInfo
\end{code}



%************************************************************************
%*									*
\subsection{The worker wrapper core}
%*									*
%************************************************************************

@mkWrapper@ is called when importing a function.  We have the type of 
the function and the name of its worker, and we want to make its body (the wrapper).

\begin{code}
mkWrapper :: Type		-- Wrapper type
	  -> Int		-- Arity
	  -> [Demand]		-- Wrapper strictness info
	  -> CprInfo            -- Wrapper cpr info
	  -> UniqSM (Id -> CoreExpr)	-- Wrapper body, missing worker Id

mkWrapper fun_ty arity demands cpr_info
  = mkWwBodies fun_ty arity demands noOneShotInfo cpr_info	`thenUs` \ (_, wrap_fn, _) ->
    returnUs wrap_fn

noOneShotInfo = repeat False
\end{code}


