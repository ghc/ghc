%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}

\begin{code}
module WorkWrap ( wwTopBinds, getWorkerId ) where

#include "HsVersions.h"

import CoreSyn
import CoreUnfold	( Unfolding, certainlySmallEnoughToInline, calcUnfoldingGuidance )
import CmdLineOpts	( opt_UF_CreationThreshold , opt_D_verbose_core2core, 
                          opt_D_dump_worker_wrapper
			)
import CoreLint		( beginPass, endPass )
import CoreUtils	( coreExprType )
import Const		( Con(..) )
import DataCon		( DataCon )
import MkId		( mkWorkerId )
import Id		( Id, getIdStrictness, setIdArity, 
			  setIdStrictness, 
			  setIdWorkerInfo, getIdCprInfo )
import VarSet
import Type		( splitAlgTyConApp_maybe )
import IdInfo		( mkStrictnessInfo, noStrictnessInfo, StrictnessInfo(..),
			  CprInfo(..), exactArity
			)
import Demand           ( wwLazy )
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
    mapUs (wwBind True{-top-level-}) top_binds `thenUs` \ top_binds2 ->
    let
	top_binds3 = map make_top_binding top_binds2
    in
    returnUs (concat top_binds3)
  where
    make_top_binding :: WwBinding -> [CoreBind]

    make_top_binding (WwLet binds) = binds
\end{code}

%************************************************************************
%*									*
\subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}
%*									*
%************************************************************************

@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...

\begin{code}
wwBind	:: Bool			-- True <=> top-level binding
	-> CoreBind
	-> UniqSM WwBinding	-- returns a WwBinding intermediate form;
				-- the caller will convert to Expr/Binding,
				-- as appropriate.

wwBind top_level (NonRec binder rhs)
  = wwExpr rhs						`thenUs` \ new_rhs ->
    tryWW True {- non-recursive -} binder new_rhs 	`thenUs` \ new_pairs ->
    returnUs (WwLet [NonRec b e | (b,e) <- new_pairs])
      -- Generated bindings must be non-recursive
      -- because the original binding was.

------------------------------

wwBind top_level (Rec pairs)
  = mapUs do_one pairs		`thenUs` \ new_pairs ->
    returnUs (WwLet [Rec (concat new_pairs)])
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
  = wwBind False{-not top-level-} bind	`thenUs` \ intermediate_bind ->
    wwExpr expr				`thenUs` \ new_expr ->
    returnUs (mash_ww_bind intermediate_bind new_expr)
  where
    mash_ww_bind (WwLet  binds)   body = mkLets binds body
    mash_ww_bind (WwCase case_fn) body = case_fn body

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
  | (non_rec &&	-- Don't split if its non-recursive and small
      certainlySmallEnoughToInline unfold_guidance
     )
	    -- No point in worker/wrappering something that is going to be
	    -- INLINEd wholesale anyway.  If the strictness analyser is run
	    -- twice, this test also prevents wrappers (which are INLINEd)
	    -- from being re-done.

  || not (do_strict_ww || do_cpr_ww) 
  = returnUs [ (fn_id, rhs) ]

  | otherwise		-- Do w/w split
  = mkWwBodies tyvars wrap_args 
	       (coreExprType body)
	       wrap_demands
	       cpr_info
                                                `thenUs` \ (wrap_fn, work_fn, work_demands) ->
    getUniqueUs					`thenUs` \ work_uniq ->
    let
	work_rhs  = work_fn body
	work_id   = mkWorkerId work_uniq fn_id (coreExprType work_rhs) `setIdStrictness`
		    (if has_strictness_info then mkStrictnessInfo (work_demands ++ remaining_arg_demands, result_bot)
	                                    else noStrictnessInfo) 

	wrap_rhs = wrap_fn work_id
	wrap_id  = fn_id `setIdStrictness` 
                         (if has_strictness_info then mkStrictnessInfo (wrap_demands ++ remaining_arg_demands, result_bot)
	                                         else noStrictnessInfo) 
                         `setIdWorkerInfo`	Just work_id
			 `setIdArity`		exactArity (length wrap_args)
		-- Add info to the wrapper:
		--	(a) we want to inline it everywhere
		-- 	(b) we want to pin on its revised strictness info
		--	(c) we pin on its worker id 
    in
    returnUs ([(work_id, work_rhs), (wrap_id, wrap_rhs)])
	-- Worker first, because wrapper mentions it
  where
    (tyvars, wrap_args, body) = collectTyAndValBinders rhs
    n_wrap_args		      = length wrap_args

    strictness_info     = getIdStrictness fn_id
    has_strictness_info = case strictness_info of
				StrictnessInfo _ _ -> True
				other		   -> False

    StrictnessInfo arg_demands result_bot = strictness_info
			
	-- NB: There maybe be more items in arg_demands than wrap_args, because
	-- the strictness info is semantic and looks through InlineMe and Scc
	-- Notes, whereas wrap_args does not
    demands_for_visible_args = take n_wrap_args arg_demands
    remaining_arg_demands    = drop n_wrap_args arg_demands

    wrap_demands | has_strictness_info = setUnpackStrategy demands_for_visible_args
		 | otherwise	       = repeat wwLazy

    do_strict_ww = has_strictness_info && worthSplitting wrap_demands result_bot

    cpr_info     = getIdCprInfo fn_id
    has_cpr_info = case cpr_info of
			CPRInfo _ -> True
			other	  -> False

    do_cpr_ww = has_cpr_info
    unfold_guidance = calcUnfoldingGuidance opt_UF_CreationThreshold rhs

-- This rather (nay! extremely!) crude function looks at a wrapper function, and
-- snaffles out the worker Id from the wrapper.
-- This is needed when we write an interface file.
-- [May 1999: we used to get the constructors too, but that's no longer
--  	      necessary, because the renamer hauls in all type decls in 
--	      their fullness.]

-- <Mar 1999 (keving)> - Well,  since the addition of the CPR transformation this function
-- got too crude!  
-- Now the worker id is stored directly in the id's Info field.  We still use this function to
-- snaffle the wrapper's constructors but I don't trust the code to find the worker id.
getWorkerId :: Id -> CoreExpr -> Id
getWorkerId wrap_id wrapper_fn
  = work_id wrapper_fn
  where

    work_id wrapper_fn
            = case get_work_id wrapper_fn of
                []   -> case work_id_try2 wrapper_fn of
                        [] -> pprPanic "getWorkerId: can't find worker id" (ppr wrap_id)
                        [id] -> id
			_    -> pprPanic "getWorkerId: found too many worker ids" (ppr wrap_id)
                [id] -> id
                _    -> pprPanic "getWorkerId: found too many worker ids" (ppr wrap_id)

    get_work_id (Lam _ body)			 = get_work_id body
    get_work_id (Case _ _ [(_,_,rhs@(Case _ _ _))])	= get_work_id rhs
    get_work_id (Case scrut _ [(_,_,rhs)])		= (get_work_id scrut) ++ (get_work_id rhs)
    get_work_id (Note _ body)			 = get_work_id body
    get_work_id (Let _ body)			 = get_work_id body
    get_work_id (App (Var work_id) _)   	 = [work_id]
    get_work_id (App fn _)   			 = get_work_id fn
    get_work_id (Var work_id)			 = []
    get_work_id other	   			 = [] 

    work_id_try2 (Lam _ body)			 = work_id_try2 body
    work_id_try2 (Note _ body)			 = work_id_try2 body
    work_id_try2 (Let _ body)			 = work_id_try2 body
    work_id_try2 (App fn _)   			 = work_id_try2 fn
    work_id_try2 (Var work_id)			 = [work_id]
    work_id_try2 other	   			 = [] 
\end{code}
