%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}

\begin{code}
#include "HsVersions.h"

module WorkWrap ( workersAndWrappers ) where

IMPORT_Trace
import Outputable
import Pretty

import Id		( getIdUniType, addIdStrictness, getIdStrictness,
			  getIdUnfolding, mkWorkerId,
			  replaceIdInfo, getIdInfo, idWantsToBeINLINEd
			)
import IdInfo		-- bits and pieces
import Maybes		( maybeToBool, Maybe(..) )
import PlainCore
import SaLib
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
import WwLib
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
workersAndWrappers :: [PlainCoreBinding] -> WwM [PlainCoreBinding]

workersAndWrappers top_binds
  = mapWw (wwBind True{-top-level-}) top_binds `thenWw` \ top_binds2 ->
    let
	top_binds3 = map make_top_binding top_binds2
    in
    returnWw (concat top_binds3)
  where
    make_top_binding :: WwBinding -> [PlainCoreBinding]

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
	-> PlainCoreBinding
	-> WwM WwBinding	-- returns a WwBinding intermediate form;
				-- the caller will convert to Expr/Binding,
				-- as appropriate.

wwBind top_level (CoNonRec binder rhs)
  = wwExpr rhs			`thenWw` \ new_rhs ->
    tryWW binder new_rhs 	`thenWw` \ new_pairs ->
    returnWw (WwLet [CoNonRec b e | (b,e) <- new_pairs])
      -- Generated bindings must be non-recursive
      -- because the original binding was.

------------------------------

wwBind top_level (CoRec pairs)
  = mapWw do_one pairs		`thenWw` \ new_pairs ->
    returnWw (WwLet [CoRec (concat new_pairs)])
  where
    do_one (binder, rhs) = wwExpr rhs 	`thenWw` \ new_rhs ->
			   tryWW binder new_rhs
\end{code}

@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.
???????????????? ToDo

\begin{code}
wwExpr :: PlainCoreExpr -> WwM PlainCoreExpr

wwExpr e@(CoVar _)	= returnWw e
wwExpr e@(CoLit _)	= returnWw e
wwExpr e@(CoCon  _ _ _) = returnWw e
wwExpr e@(CoPrim _ _ _) = returnWw e

wwExpr (CoLam binders expr)
  = wwExpr expr			`thenWw` \ new_expr ->
    returnWw (CoLam binders new_expr)

wwExpr (CoTyLam ty expr)
  = wwExpr expr			`thenWw` \ new_expr ->
    returnWw (CoTyLam ty new_expr)

wwExpr (CoApp e1 e2)
  = wwExpr e1			`thenWw` \ new_e1 ->
    returnWw (CoApp new_e1 e2)

wwExpr (CoTyApp expr ty)
  = wwExpr expr			`thenWw` \ new_expr ->
    returnWw (CoTyApp new_expr ty)

wwExpr (CoSCC cc expr)
  = wwExpr expr			`thenWw` \ new_expr ->
    returnWw (CoSCC cc new_expr)

wwExpr (CoLet bind expr)
  = wwBind False{-not top-level-} bind	`thenWw` \ intermediate_bind ->
    wwExpr expr				`thenWw` \ new_expr ->
    returnWw (mash_ww_bind intermediate_bind new_expr)
  where
    mash_ww_bind (WwLet  binds)   body = mkCoLetsNoUnboxed binds body
    mash_ww_bind (WwCase case_fn) body = case_fn body

wwExpr (CoCase expr alts)
  = wwExpr expr				`thenWw` \ new_expr ->
    ww_alts alts			`thenWw` \ new_alts ->
    returnWw (CoCase new_expr new_alts)
  where
    ww_alts (CoAlgAlts alts deflt)
      = mapWw ww_alg_alt alts		`thenWw` \ new_alts ->
	ww_deflt deflt			`thenWw` \ new_deflt ->
	returnWw (CoAlgAlts new_alts new_deflt)

    ww_alts (CoPrimAlts alts deflt)
      = mapWw ww_prim_alt alts		`thenWw` \ new_alts ->
	ww_deflt deflt			`thenWw` \ new_deflt ->
	returnWw (CoPrimAlts new_alts new_deflt)

    ww_alg_alt (con, binders, rhs)
      =	wwExpr rhs			`thenWw` \ new_rhs ->
	returnWw (con, binders, new_rhs)

    ww_prim_alt (lit, rhs)
      = wwExpr rhs			`thenWw` \ new_rhs ->
	returnWw (lit, new_rhs)

    ww_deflt CoNoDefault
      = returnWw CoNoDefault

    ww_deflt (CoBindDefault binder rhs)
      = wwExpr rhs			`thenWw` \ new_rhs ->
	returnWw (CoBindDefault binder new_rhs)
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
	-> PlainCoreExpr		-- the bound rhs; its innards
					--   are already ww'd
	-> WwM [(Id, PlainCoreExpr)]	-- either *one* or *two* pairs;
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
	     (tyvars, args, body) = digForLambdas rhs
	     body_ty		  = typeOfCoreExpr body
	in
	uniqSMtoWwM (mkWwBodies body_ty tyvars args args_info) `thenWw` \ result ->
	case result of

	  Nothing -> 	-- Very peculiar. This can only happen if we hit an 
			-- abstract type, which we shouldn't have since we've
			-- constructed the args_info in this module!
			
			-- False. We might hit the all-args-absent-and-the-
			-- body-is-unboxed case.  A Nothing is legit. (WDP 94/10)
			do_nothing

	  Just (wrapper_w_hole, worker_w_hole, worker_strictness, worker_ty_w_hole) ->

		-- Terrific!  It worked!
	    getUniqueWw		`thenWw` \ worker_uniq ->
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
	    returnWw [ (worker_id,  worker_rhs),   -- worker comes first
		       (wrapper_id, wrapper_rhs) ] -- because wrapper mentions it
  where
    do_nothing = returnWw [ (fn_id, rhs) ]
\end{code}
