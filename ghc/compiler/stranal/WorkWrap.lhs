%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}

\begin{code}
#include "HsVersions.h"

module WorkWrap ( workersAndWrappers ) where

import Ubiq{-uitous-}

import CoreSyn
import CoreUnfold	( UnfoldingGuidance(..) )
import CoreUtils	( coreExprType )
import Id		( idWantsToBeINLINEd, getIdStrictness, mkWorkerId,
			  getIdInfo
			)
import IdInfo		( noIdInfo, addInfo_UF, indicatesWorker,
			  mkStrictnessInfo, StrictnessInfo(..)
			)
import SaLib
import UniqSupply	( returnUs, thenUs, mapUs, getUnique, UniqSM(..) )
import WwLib
import Util		( panic{-ToDo:rm-} )

replaceIdInfo = panic "WorkWrap.replaceIdInfo (ToDo)"
iWantToBeINLINEd = panic "WorkWrap.iWantToBeINLINEd (ToDo)"
\end{code}

We take Core bindings whose binders have their strictness attached (by
the front-end of the strictness analyser), and we return some
``plain'' bindings which have been worker/wrapper-ified, meaning:
\begin{enumerate}
\item
Functions have been split into workers and wrappers where appropriate;
\item
Binders' @IdInfos@ have been updated to reflect the existence
of these workers/wrappers (this is where we get STRICTNESS pragma
info for exported values).
\end{enumerate}

\begin{code}
workersAndWrappers :: [CoreBinding] -> UniqSM [CoreBinding]

workersAndWrappers top_binds
  = mapUs (wwBind True{-top-level-}) top_binds `thenUs` \ top_binds2 ->
    let
	top_binds3 = map make_top_binding top_binds2
    in
    returnUs (concat top_binds3)
  where
    make_top_binding :: WwBinding -> [CoreBinding]

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
	-> CoreBinding
	-> UniqSM WwBinding	-- returns a WwBinding intermediate form;
				-- the caller will convert to Expr/Binding,
				-- as appropriate.

wwBind top_level (NonRec binder rhs)
  = wwExpr rhs			`thenUs` \ new_rhs ->
    tryWW binder new_rhs 	`thenUs` \ new_pairs ->
    returnUs (WwLet [NonRec b e | (b,e) <- new_pairs])
      -- Generated bindings must be non-recursive
      -- because the original binding was.

------------------------------

wwBind top_level (Rec pairs)
  = mapUs do_one pairs		`thenUs` \ new_pairs ->
    returnUs (WwLet [Rec (concat new_pairs)])
  where
    do_one (binder, rhs) = wwExpr rhs 	`thenUs` \ new_rhs ->
			   tryWW binder new_rhs
\end{code}

@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.
???????????????? ToDo

\begin{code}
wwExpr :: CoreExpr -> UniqSM CoreExpr

wwExpr e@(Var _)    = returnUs e
wwExpr e@(Lit _)    = returnUs e
wwExpr e@(Con  _ _) = returnUs e
wwExpr e@(Prim _ _) = returnUs e

wwExpr (Lam binder expr)
  = wwExpr expr			`thenUs` \ new_expr ->
    returnUs (Lam binder new_expr)

wwExpr (App f a)
  = wwExpr f			`thenUs` \ new_f ->
    returnUs (App new_f a)

wwExpr (SCC cc expr)
  = wwExpr expr			`thenUs` \ new_expr ->
    returnUs (SCC cc new_expr)

wwExpr (Let bind expr)
  = wwBind False{-not top-level-} bind	`thenUs` \ intermediate_bind ->
    wwExpr expr				`thenUs` \ new_expr ->
    returnUs (mash_ww_bind intermediate_bind new_expr)
  where
    mash_ww_bind (WwLet  binds)   body = mkCoLetsNoUnboxed binds body
    mash_ww_bind (WwCase case_fn) body = case_fn body

wwExpr (Case expr alts)
  = wwExpr expr				`thenUs` \ new_expr ->
    ww_alts alts			`thenUs` \ new_alts ->
    returnUs (Case new_expr new_alts)
  where
    ww_alts (AlgAlts alts deflt)
      = mapUs ww_alg_alt alts		`thenUs` \ new_alts ->
	ww_deflt deflt			`thenUs` \ new_deflt ->
	returnUs (AlgAlts new_alts new_deflt)

    ww_alts (PrimAlts alts deflt)
      = mapUs ww_prim_alt alts		`thenUs` \ new_alts ->
	ww_deflt deflt			`thenUs` \ new_deflt ->
	returnUs (PrimAlts new_alts new_deflt)

    ww_alg_alt (con, binders, rhs)
      =	wwExpr rhs			`thenUs` \ new_rhs ->
	returnUs (con, binders, new_rhs)

    ww_prim_alt (lit, rhs)
      = wwExpr rhs			`thenUs` \ new_rhs ->
	returnUs (lit, new_rhs)

    ww_deflt NoDefault
      = returnUs NoDefault

    ww_deflt (BindDefault binder rhs)
      = wwExpr rhs			`thenUs` \ new_rhs ->
	returnUs (BindDefault binder new_rhs)
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
tryWW	:: Id				-- the fn binder
	-> CoreExpr		-- the bound rhs; its innards
					--   are already ww'd
	-> UniqSM [(Id, CoreExpr)]	-- either *one* or *two* pairs;
					-- if one, then no worker (only
					-- the orig "wrapper" lives on);
					-- if two, then a worker and a
					-- wrapper.
tryWW fn_id rhs
  | idWantsToBeINLINEd fn_id
    -- No point in worker/wrappering something that is going to be
    -- INLINEd wholesale anyway.  If the strictness analyser is run
    -- twice, this test also prevents wrappers (which are INLINEd)
    -- from being re-done.
  = do_nothing

  | otherwise
  = case (getIdStrictness fn_id) of

      NoStrictnessInfo    -> do_nothing
      BottomGuaranteed    -> do_nothing
      StrictnessInfo [] _ -> do_nothing -- V weird (but possible?)

      StrictnessInfo args_info _ ->
	if not (indicatesWorker args_info) then
	    do_nothing
	else

	-- OK, it looks as if a worker is worth a try
	let
	     (uvars, tyvars, args, body) = collectBinders rhs
	     body_ty			 = coreExprType body
	in
	mkWwBodies body_ty tyvars args args_info `thenUs` \ result ->
	case result of

	  Nothing -> 	-- Very peculiar. This can only happen if we hit an
			-- abstract type, which we shouldn't have since we've
			-- constructed the args_info in this module!

			-- False. We might hit the all-args-absent-and-the-
			-- body-is-unboxed case.  A Nothing is legit. (WDP 94/10)
			do_nothing

	  Just (wrapper_w_hole, worker_w_hole, worker_strictness, worker_ty_w_hole) ->

		-- Terrific!  It worked!
	    getUnique		`thenUs` \ worker_uniq ->
	    let
		worker_ty   = worker_ty_w_hole body_ty

		worker_id   = mkWorkerId worker_uniq fn_id worker_ty
				(noIdInfo `addInfo` worker_strictness)

		wrapper_rhs = wrapper_w_hole worker_id
		worker_rhs  = worker_w_hole body

		revised_strictness_info
		  = -- We know the basic strictness info already, but
		    -- we need to slam in the exact identity of the
		    -- worker Id:
		    mkStrictnessInfo args_info (Just worker_id)

		wrapper_id  = fn_id `replaceIdInfo`
			      (getIdInfo fn_id		`addInfo`
			       revised_strictness_info	`addInfo_UF`
			       iWantToBeINLINEd UnfoldAlways)
		-- NB! the "iWantToBeINLINEd" part adds an INLINE pragma to
		-- the wrapper, which is of course what we want.
	    in
	    returnUs [ (worker_id,  worker_rhs),   -- worker comes first
		       (wrapper_id, wrapper_rhs) ] -- because wrapper mentions it
  where
    do_nothing = returnUs [ (fn_id, rhs) ]
\end{code}
