%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[SCCfinal]{Modify and collect code generation for final STG program}

This is now a sort-of-normal STG-to-STG pass (WDP 94/06), run by stg2stg.

* Traverses the STG program collecting the cost centres. These are
  required to declare the cost centres at the start of code
  generation.

  Note: because of cross-module unfolding, some of these cost centres
  may be from other modules.  But will still have to give them
  "extern" declarations.

* Puts on CAF cost-centres if the user has asked for individual CAF
  cost-centres.

* Ditto for individual DICT cost-centres.

* Boxes top-level inherited functions passed as arguments.

* "Distributes" given cost-centres to all as-yet-unmarked RHSs.

\begin{code}
#include "HsVersions.h"

module SCCfinal ( stgMassageForProfiling ) where

IMP_Ubiq(){-uitous-}

import StgSyn

import CmdLineOpts	( opt_AutoSccsOnIndividualCafs,
			  opt_CompilingPrelude
			)
import CostCentre	-- lots of things
import Id		( idType, mkSysLocal, emptyIdSet )
import Maybes		( maybeToBool )
import SrcLoc		( mkUnknownSrcLoc )
import Type		( splitSigmaTy, getFunTy_maybe )
import UniqSupply	( getUnique, splitUniqSupply )
import Util		( removeDups, assertPanic )

infixr 9 `thenMM`, `thenMM_`
\end{code}

\begin{code}
type CollectedCCs = ([CostCentre],	-- locally defined ones
		     [CostCentre])	-- ones needing "extern" decls

stgMassageForProfiling
	:: FAST_STRING -> FAST_STRING	-- module name, group name
	-> UniqSupply		    	-- unique supply
	-> [StgBinding]		    	-- input
	-> (CollectedCCs, [StgBinding])

stgMassageForProfiling mod_name grp_name us stg_binds
  = let
	((local_ccs, extern_ccs),
	 stg_binds2)
	  = initMM mod_name us (mapMM do_top_binding stg_binds)

	fixed_ccs
	  = if do_auto_sccs_on_cafs || doing_prelude
	    then [] -- don't need "all CAFs" CC (for Prelude, we use PreludeCC)
	    else [all_cafs_cc]

	local_ccs_no_dups  = fst (removeDups cmpCostCentre local_ccs)
	extern_ccs_no_dups = fst (removeDups cmpCostCentre extern_ccs)
    in
    ((fixed_ccs ++ local_ccs_no_dups, extern_ccs_no_dups), stg_binds2)
  where
    do_auto_sccs_on_cafs  = opt_AutoSccsOnIndividualCafs  -- only use!
    doing_prelude	  = opt_CompilingPrelude

    all_cafs_cc = if doing_prelude
		  then preludeCafsCostCentre
		  else mkAllCafsCC mod_name grp_name

    ----------
    do_top_binding :: StgBinding -> MassageM StgBinding

    do_top_binding (StgNonRec b rhs)
      = do_top_rhs b rhs 		`thenMM` \ rhs' ->
	returnMM (StgNonRec b rhs')

    do_top_binding (StgRec pairs)
      = mapMM do_pair pairs		`thenMM` \ pairs2 ->
	returnMM (StgRec pairs2)
      where
	do_pair (b, rhs)
	   = do_top_rhs b rhs	`thenMM` \ rhs2 ->
	     returnMM (b, rhs2)

    ----------
    do_top_rhs :: Id -> StgRhs -> MassageM StgRhs

    do_top_rhs binder (StgRhsClosure rhs_cc bi fv u [] (StgSCC ty cc (StgCon con args lvs)))
	-- top-level _scc_ around nothing but static data; toss it -- it's pointless
      = returnMM (StgRhsCon dontCareCostCentre con args)

    do_top_rhs binder (StgRhsClosure rhs_cc bi fv u [] (StgSCC ty cc expr))
	-- Top level CAF with explicit scc expression.  Attach CAF
	-- cost centre to StgRhsClosure and collect.
      = let
	   calved_cc = cafifyCC cc
	in
	collectCC calved_cc	`thenMM_`
	set_prevailing_cc calved_cc (
	    do_expr expr
	)			`thenMM`  \ expr' ->
	returnMM (StgRhsClosure calved_cc bi fv u [] expr')

    do_top_rhs binder (StgRhsClosure cc bi fv u [] body)
      | noCostCentreAttached cc || currentOrSubsumedCosts cc
	-- Top level CAF without a cost centre attached: Collect
	-- cost centre with binder name, if collecting CAFs.
      = let
	    (did_something, cc2)
	      = if do_auto_sccs_on_cafs then
		   (True, mkAutoCC binder mod_name grp_name IsCafCC)
		else
		   (False, all_cafs_cc)
	in
	(if did_something
	 then collectCC cc2
	 else nopMM)		`thenMM_`
	set_prevailing_cc cc2 (
	    do_expr body
	)			`thenMM`  \body2 ->
	returnMM (StgRhsClosure cc2 bi fv u [] body2)

    do_top_rhs binder (StgRhsClosure _ bi fv u args body@(StgSCC ty cc expr))
	-- We blindly use the cc off the _scc_
      = set_prevailing_cc cc (
	    do_expr body
	)		`thenMM` \ body2 ->
	returnMM (StgRhsClosure cc bi fv u args body2)

    do_top_rhs binder (StgRhsClosure cc bi fv u args body)
      = let
	    cc2 = if noCostCentreAttached cc
		  then subsumedCosts -- it's not a thunk; it is top-level & arity > 0
		  else cc
	in
	set_prevailing_cc cc2 (
	    do_expr body
	)		`thenMM` \ body' ->
	returnMM (StgRhsClosure cc2 bi fv u args body')

    do_top_rhs binder (StgRhsCon cc con args)
      = returnMM (StgRhsCon dontCareCostCentre con args)
	-- Top-level (static) data is not counted in heap
	-- profiles; nor do we set CCC from it; so we
	-- just slam in dontCareCostCentre

    ------
    do_expr :: StgExpr -> MassageM StgExpr

    do_expr (StgApp fn args lvs)
      = boxHigherOrderArgs (StgApp fn) args lvs

    do_expr (StgCon con args lvs)
      = boxHigherOrderArgs (StgCon con) args lvs

    do_expr (StgPrim op args lvs)
      = boxHigherOrderArgs (StgPrim op) args lvs

    do_expr (StgSCC ty cc expr)	-- Ha, we found a cost centre!
      = collectCC cc		`thenMM_`
	set_prevailing_cc cc (
	    do_expr expr
	)			`thenMM`  \ expr' ->
	returnMM (StgSCC ty cc expr')

    do_expr (StgCase expr fv1 fv2 uniq alts)
      = do_expr expr		`thenMM` \ expr' ->
	do_alts alts		`thenMM` \ alts' ->
	returnMM (StgCase expr' fv1 fv2 uniq alts')
      where
	do_alts (StgAlgAlts ty alts def)
	  = mapMM do_alt alts 	`thenMM` \ alts' ->
	    do_deflt def	`thenMM` \ def' ->
	    returnMM (StgAlgAlts ty alts' def')
	  where
	    do_alt (id, bs, use_mask, e)
	      = do_expr e `thenMM` \ e' ->
		returnMM (id, bs, use_mask, e')

	do_alts (StgPrimAlts ty alts def)
	  = mapMM do_alt alts	`thenMM` \ alts' ->
	    do_deflt def	`thenMM` \ def' ->
	    returnMM (StgPrimAlts ty alts' def')
	  where
	    do_alt (l,e)
	      = do_expr e `thenMM` \ e' ->
		returnMM (l,e')

	do_deflt StgNoDefault = returnMM StgNoDefault
	do_deflt (StgBindDefault b is_used e)
	  = do_expr e			`thenMM` \ e' ->
	    returnMM (StgBindDefault b is_used e')

    do_expr (StgLet b e)
      = set_prevailing_cc_maybe useCurrentCostCentre (
	do_binding b		`thenMM` \ b' ->
	do_expr e		`thenMM` \ e' ->
	returnMM (StgLet b' e') )

    do_expr (StgLetNoEscape lvs1 lvs2 rhs body)
      = set_prevailing_cc_maybe useCurrentCostCentre (
	do_binding rhs		`thenMM` \ rhs' ->
	do_expr body		`thenMM` \ body' ->
	returnMM (StgLetNoEscape lvs1 lvs2 rhs' body') )

    ----------
    do_binding :: StgBinding -> MassageM StgBinding

    do_binding (StgNonRec b rhs)
      = do_rhs rhs 			`thenMM` \ rhs' ->
	returnMM (StgNonRec b rhs')

    do_binding (StgRec pairs)
      = mapMM do_pair pairs `thenMM` \ new_pairs ->
	returnMM (StgRec new_pairs)
      where
	do_pair (b, rhs)
	  = do_rhs rhs	`thenMM` \ rhs' ->
	    returnMM (b, rhs')

    do_rhs :: StgRhs -> MassageM StgRhs
	-- We play much the same game as we did in do_top_rhs above;
	-- but we don't have to worry about cafifying, etc.
	-- (ToDo: consolidate??)

{- Patrick says NO: it will mess up our counts (WDP 95/07)
    do_rhs (StgRhsClosure _ bi fv u [] (StgSCC _ cc (StgCon con args lvs)))
      = collectCC cc `thenMM_`
	returnMM (StgRhsCon cc con args)
-}

    do_rhs (StgRhsClosure _ bi fv u args body@(StgSCC _ cc _))
      = set_prevailing_cc cc (
	    do_expr body
	)			    `thenMM` \ body' ->
	returnMM (StgRhsClosure cc bi fv u args body')

    do_rhs (StgRhsClosure cc bi fv u args body)
      = use_prevailing_cc_maybe cc  `thenMM` \ cc2 ->
	set_prevailing_cc cc2 (
	    do_expr body
	)			    `thenMM` \ body' ->
	returnMM (StgRhsClosure cc2 bi fv u args body')

    do_rhs (StgRhsCon cc con args)
      = use_prevailing_cc_maybe cc  `thenMM` \ cc2 ->
	returnMM (StgRhsCon cc2 con args)
      -- ToDo: Box args (if lex) Pass back let binding???
      -- Nope: maybe later? WDP 94/06
\end{code}

%************************************************************************
%*									*
\subsection{Boxing higher-order args}
%*									*
%************************************************************************

\begin{code}
boxHigherOrderArgs
    :: ([StgArg] -> StgLiveVars -> StgExpr)
	-- An application lacking its arguments and live-var info
    -> [StgArg]	-- arguments which we might box
    -> StgLiveVars	-- live var info, which we do *not* try
			-- to maintain/update (setStgVarInfo will
			-- do that)
    -> MassageM StgExpr

boxHigherOrderArgs almost_expr args live_vars
  = mapAccumMM do_arg [] args	`thenMM` \ (let_bindings, new_args) ->
    get_prevailing_cc		`thenMM` \ cc ->
    returnMM (foldr (mk_stg_let cc) (almost_expr new_args live_vars) let_bindings)
  where
    ---------------
    do_arg bindings atom@(StgLitArg _) = returnMM (bindings, atom)

    do_arg bindings atom@(StgVarArg old_var)
      = let
	    var_type = idType old_var
	in
	if not (is_fun_type var_type) then
	    returnMM (bindings, atom) -- easy
	else
	    -- make a trivial let-binding for the higher-order guy
	    getUniqueMM		`thenMM` \ uniq ->
	    let
		new_var = mkSysLocal SLIT("ho") uniq var_type mkUnknownSrcLoc
	    in
	    returnMM ( (new_var, old_var) : bindings, StgVarArg new_var )
      where
	is_fun_type ty
	  = case (splitSigmaTy ty) of { (_, _, tau_ty) ->
	    maybeToBool (getFunTy_maybe tau_ty) }

    ---------------
    mk_stg_let :: CostCentre -> (Id, Id) -> StgExpr -> StgExpr

    mk_stg_let cc (new_var, old_var) body
      = let
	    rhs_body = StgApp (StgVarArg old_var) [{-no args-}] bOGUS_LVs

	    rhs = StgRhsClosure cc
			stgArgOcc -- safe...
			[{-junk-}] Updatable [{-no args-}] rhs_body
	in
	StgLet (StgNonRec new_var rhs) body
      where
	bOGUS_LVs = emptyIdSet -- easier to print than: panic "mk_stg_let: LVs"
\end{code}

%************************************************************************
%*									*
\subsection{Boring monad stuff for this}
%*									*
%************************************************************************

\begin{code}
type MassageM result
  =  FAST_STRING	-- module name
  -> CostCentre		-- prevailing CostCentre
			-- if none, subsumedCosts at top-level
			-- useCurrentCostCentre at nested levels
  -> UniqSupply
  -> CollectedCCs
  -> (CollectedCCs, result)

-- the initUs function also returns the final UniqueSupply and CollectedCCs

initMM :: FAST_STRING	-- module name, which we may consult
       -> UniqSupply
       -> MassageM a
       -> (CollectedCCs, a)

initMM mod_name init_us m = m mod_name subsumedCosts{-top-level-} init_us ([],[])

thenMM  :: MassageM a -> (a -> MassageM b) -> MassageM b
thenMM_ :: MassageM a -> (MassageM b) -> MassageM b

thenMM expr cont mod scope_cc us ccs
  = case splitUniqSupply us	of { (s1, s2) ->
    case (expr mod scope_cc s1 ccs)    	of { (ccs2, result) ->
    cont result mod scope_cc s2 ccs2 }}

thenMM_ expr cont mod scope_cc us ccs
  = case splitUniqSupply us	of { (s1, s2) ->
    case (expr mod scope_cc s1 ccs)    	of { (ccs2, _) ->
    cont mod scope_cc s2 ccs2 }}

returnMM :: a -> MassageM a
returnMM result mod scope_cc us ccs = (ccs, result)

nopMM :: MassageM ()
nopMM mod scope_cc us ccs = (ccs, ())

mapMM :: (a -> MassageM b) -> [a] -> MassageM [b]

mapMM f [] = returnMM []
mapMM f (m:ms)
  = f m		`thenMM` \ r  ->
    mapMM f ms	`thenMM` \ rs ->
    returnMM (r:rs)

mapAccumMM :: (acc -> x -> MassageM (acc, y)) -> acc -> [x] -> MassageM (acc, [y])

mapAccumMM f b [] = returnMM (b, [])
mapAccumMM f b (m:ms)
  = f b m		`thenMM` \ (b2, r)  ->
    mapAccumMM f b2 ms	`thenMM` \ (b3, rs) ->
    returnMM (b3, r:rs)

getUniqueMM :: MassageM Unique
getUniqueMM mod scope_cc us ccs = (ccs, getUnique us)
\end{code}

\begin{code}
set_prevailing_cc, set_prevailing_cc_maybe
	:: CostCentre -> MassageM a -> MassageM a

set_prevailing_cc cc_to_set_to action mod scope_cc us ccs
  = action mod cc_to_set_to us ccs
    -- set unconditionally

set_prevailing_cc_maybe cc_to_set_to action mod scope_cc us ccs
  = let
	-- used when switching from top-level to nested
	-- scope; if we were chugging along as "subsumed",
	-- we change to the new thing; otherwise we
	-- keep what we had.

	cc_to_use
	  = if (costsAreSubsumed scope_cc)
	    then cc_to_set_to
	    else scope_cc   -- carry on as before
    in
    action mod cc_to_use us ccs

get_prevailing_cc :: MassageM CostCentre
get_prevailing_cc mod scope_cc us ccs = (ccs, scope_cc)

use_prevailing_cc_maybe :: CostCentre -> MassageM CostCentre

use_prevailing_cc_maybe cc_to_try mod scope_cc us ccs
  = let
	cc_to_use
	  = if not (noCostCentreAttached   cc_to_try
		 || currentOrSubsumedCosts cc_to_try) then
		cc_to_try
	    else
		uncalved_scope_cc
		-- carry on as before, but be sure it
		-- isn't marked as CAFish (we're
		-- crossing a lambda...)
    in
    (ccs, cc_to_use)
  where
    uncalved_scope_cc = unCafifyCC scope_cc
\end{code}

\begin{code}
collectCC :: CostCentre -> MassageM ()

collectCC cc mod_name scope_cc us (local_ccs, extern_ccs)
  = ASSERT(not (noCostCentreAttached cc))
    ASSERT(not (currentOrSubsumedCosts cc))
    if (cc `ccFromThisModule` mod_name) then
	((cc : local_ccs, extern_ccs), ())
    else -- must declare it "extern"
	((local_ccs, cc : extern_ccs), ())
\end{code}
