%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}

\begin{code}
module WorkWrap ( wwTopBinds, getWorkerIdAndCons ) where

#include "HsVersions.h"

import CoreSyn
import CoreUnfold	( Unfolding, certainlySmallEnoughToInline, calcUnfoldingGuidance )
import CmdLineOpts	( opt_UnfoldingCreationThreshold, opt_D_verbose_core2core, 
                          opt_D_dump_worker_wrapper )
import CoreLint		( beginPass, endPass )
import CoreUtils	( coreExprType )
import Const		( Con(..) )
import DataCon		( DataCon )
import MkId		( mkWorkerId )
import Id		( Id, getIdStrictness,
			  setIdStrictness, setInlinePragma, idWantsToBeINLINEd,
			  setIdWorkerInfo, getIdCprInfo )
import VarSet
import Type		( splitAlgTyConApp_maybe )
import IdInfo		( mkStrictnessInfo, noStrictnessInfo, StrictnessInfo(..),
			  InlinePragInfo(..), CprInfo(..) )
import Demand           ( wwLazy )
import SaLib
import UniqSupply	( UniqSupply, initUs, returnUs, thenUs, mapUs, getUniqueUs, UniqSM )
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
  = initUs us $
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
  |  idWantsToBeINLINEd fn_id 
  || (non_rec &&	-- Don't split if its non-recursive and small
      certainlySmallEnoughToInline fn_id unfold_guidance
     )
	    -- No point in worker/wrappering something that is going to be
	    -- INLINEd wholesale anyway.  If the strictness analyser is run
	    -- twice, this test also prevents wrappers (which are INLINEd)
	    -- from being re-done.

  || not (do_strict_ww || do_cpr_ww) 
  = returnUs [ (fn_id, rhs) ]

  | otherwise		-- Do w/w split
  = let
	(tyvars, wrap_args, body) = collectTyAndValBinders rhs
    in
    mkWwBodies tyvars wrap_args 
	       (coreExprType body)
	       revised_wrap_args_info
	       cpr_info
                                                `thenUs` \ (wrap_fn, work_fn, work_demands) ->
    getUniqueUs					`thenUs` \ work_uniq ->
    let
	work_rhs  = work_fn body
	work_id   = mkWorkerId work_uniq fn_id (coreExprType work_rhs) `setIdStrictness`
		    (if do_strict_ww then mkStrictnessInfo (work_demands, result_bot)
                                     else noStrictnessInfo) 

	wrap_rhs = wrap_fn work_id
	wrap_id  = fn_id `setIdStrictness` 
                         (if do_strict_ww then mkStrictnessInfo (revised_wrap_args_info, result_bot)
                                          else noStrictnessInfo) 
                         `setIdWorkerInfo` (Just work_id)
			 `setInlinePragma` IWantToBeINLINEd
		-- Add info to the wrapper:
		--	(a) we want to inline it everywhere
		-- 	(b) we want to pin on its revised strictness info
		--	(c) we pin on its worker id 
    in
    returnUs ([(work_id, work_rhs), (wrap_id, wrap_rhs)])
	-- Worker first, because wrapper mentions it
  where
    strictness_info     = getIdStrictness fn_id
    has_strictness_info = case strictness_info of
				StrictnessInfo _ _ -> True
				other		   -> False

    StrictnessInfo wrap_args_info result_bot = strictness_info
			
    revised_wrap_args_info = if has_strictness_info 
                               then setUnpackStrategy wrap_args_info
                               else repeat wwLazy


    -- If we are going to split for CPR purposes anyway,  then 
    -- we may as well do the strictness transformation
    do_strict_ww = has_strictness_info && (do_cpr_ww || 
					   worthSplitting revised_wrap_args_info)

    cpr_info     = getIdCprInfo fn_id
    has_cpr_info = case cpr_info of
				CPRInfo _ -> True
				other	  -> False

    do_cpr_ww = has_cpr_info

    unfold_guidance = calcUnfoldingGuidance opt_UnfoldingCreationThreshold rhs

-- This rather (nay! extremely!) crude function looks at a wrapper function, and
-- snaffles out (a) the worker Id and (b) constructors needed to 
-- make the wrapper.
-- These are needed when we write an interface file.

-- <Mar 1999 (keving)> - Well,  since the addition of the CPR transformation this function
-- got too crude!  
-- Now the worker id is stored directly in the id's Info field.  We still use this function to
-- snaffle the wrapper's constructors but I don't trust the code to find the worker id.
getWorkerIdAndCons :: Id -> CoreExpr -> (Id, UniqSet DataCon)
getWorkerIdAndCons wrap_id wrapper_fn
  = (work_id wrapper_fn, get_cons wrapper_fn)
  where

    work_id wrapper_fn
            = case get_work_id wrapper_fn of
                []   -> case work_id_try2 wrapper_fn of
                        [] -> pprPanic "getWorkerIdAndCons: can't find worker id" (ppr wrap_id)
                        [id] -> id
			_    -> pprPanic "getWorkerIdAndCons: found too many worker ids" (ppr wrap_id)
                [id] -> id
                _    -> pprPanic "getWorkerIdAndCons: found too many worker ids" (ppr wrap_id)

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

    get_cons (Lam _ body)			= get_cons body
    get_cons (Let (NonRec _ rhs) body)		= get_cons rhs `unionUniqSets` get_cons body

    get_cons (Case e _ [(DataCon dc,_,rhs)]) 	= (get_cons e `unionUniqSets` get_cons rhs)
						  `addOneToUniqSet` dc

	-- Coercions don't mention the construtor now,
	-- but we must still put the constructor in the interface
	-- file so that the RHS of the newtype decl is imported
    get_cons (Note (Coerce to_ty from_ty) body)
	= get_cons body `addOneToUniqSet` con
	where
	  con = case splitAlgTyConApp_maybe from_ty of
			Just (_, _, [con]) -> con
			other		   -> pprPanic "getWorkerIdAndCons" (ppr to_ty)

    get_cons other = emptyUniqSet
\end{code}
