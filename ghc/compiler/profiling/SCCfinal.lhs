%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
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
module SCCfinal ( stgMassageForProfiling ) where

#include "HsVersions.h"

import StgSyn

import Packages		( HomeModules )
import StaticFlags	( opt_AutoSccsOnIndividualCafs )
import CostCentre	-- lots of things
import Id		( Id )
import Module		( Module )
import UniqSupply	( uniqFromSupply, splitUniqSupply, UniqSupply )
import Unique           ( Unique )
import VarSet
import ListSetOps	( removeDups )
import Outputable	

infixr 9 `thenMM`, `thenMM_`
\end{code}

\begin{code}
stgMassageForProfiling
	:: HomeModules
	-> Module			-- module name
	-> UniqSupply		    	-- unique supply
	-> [StgBinding]		    	-- input
	-> (CollectedCCs, [StgBinding])

stgMassageForProfiling pdeps mod_name us stg_binds
  = let
	((local_ccs, extern_ccs, cc_stacks),
	 stg_binds2)
	  = initMM mod_name us (do_top_bindings stg_binds)

	(fixed_ccs, fixed_cc_stacks)
	  = if opt_AutoSccsOnIndividualCafs
	    then ([],[])  -- don't need "all CAFs" CC 
			  -- (for Prelude, we use PreludeCC)
	    else ([all_cafs_cc], [all_cafs_ccs])

	local_ccs_no_dups  = fst (removeDups cmpCostCentre local_ccs)
	extern_ccs_no_dups = fst (removeDups cmpCostCentre extern_ccs)
    in
    ((fixed_ccs ++ local_ccs_no_dups, 
      extern_ccs_no_dups, 
      fixed_cc_stacks ++ cc_stacks), stg_binds2)
  where

    all_cafs_cc  = mkAllCafsCC mod_name
    all_cafs_ccs = mkSingletonCCS all_cafs_cc

    ----------
    do_top_bindings :: [StgBinding] -> MassageM [StgBinding]

    do_top_bindings [] = returnMM []

    do_top_bindings (StgNonRec b rhs : bs) 
      = do_top_rhs b rhs 		`thenMM` \ rhs' ->
	addTopLevelIshId b (
	   do_top_bindings bs `thenMM` \bs' ->
	   returnMM (StgNonRec b rhs' : bs')
	)

    do_top_bindings (StgRec pairs : bs)
      = addTopLevelIshIds binders (
	   mapMM do_pair pairs		`thenMM` \ pairs2 ->
	   do_top_bindings bs `thenMM` \ bs' ->
	   returnMM (StgRec pairs2 : bs')
	)
      where
	binders = map fst pairs
	do_pair (b, rhs) 
	   = do_top_rhs b rhs	`thenMM` \ rhs2 ->
	     returnMM (b, rhs2)

    ----------
    do_top_rhs :: Id -> StgRhs -> MassageM StgRhs

    do_top_rhs binder (StgRhsClosure _ bi fv u srt [] (StgSCC cc (StgConApp con args)))
      | not (isSccCountCostCentre cc) && not (isDllConApp pdeps con args)
	-- Trivial _scc_ around nothing but static data
	-- Eliminate _scc_ ... and turn into StgRhsCon

	-- isDllConApp checks for LitLit args too
      = returnMM (StgRhsCon dontCareCCS con args)

{- Can't do this one with cost-centre stacks:  --SDM
    do_top_rhs binder (StgRhsClosure no_cc bi fv u [] (StgSCC ty cc expr))
      | (noCCSAttached no_cc || currentOrSubsumedCCS no_cc)
        && not (isSccCountCostCentre cc)
	-- Top level CAF without a cost centre attached
	-- Attach and collect cc of trivial _scc_ in body
      = collectCC cc					`thenMM_`
	set_prevailing_cc cc (do_expr expr)		`thenMM`  \ expr' ->
        returnMM (StgRhsClosure cc bi fv u [] expr')
-}

    do_top_rhs binder (StgRhsClosure no_cc bi fv u srt [] body)
      | noCCSAttached no_cc || currentOrSubsumedCCS no_cc
	-- Top level CAF without a cost centre attached
	-- Attach CAF cc (collect if individual CAF ccs)
      = (if opt_AutoSccsOnIndividualCafs 
		then let cc = mkAutoCC binder mod_name CafCC
			 ccs = mkSingletonCCS cc
		     in
		     collectCC  cc  `thenMM_`
		     collectCCS ccs `thenMM_`
		     returnMM ccs
		else 
		     returnMM all_cafs_ccs)		`thenMM`  \ caf_ccs ->
	   set_prevailing_cc caf_ccs (do_expr body)	`thenMM`  \ body' ->
           returnMM (StgRhsClosure caf_ccs bi fv u srt [] body')

    do_top_rhs binder (StgRhsClosure cc bi fv u srt [] body)
	-- Top level CAF with cost centre attached
	-- Should this be a CAF cc ??? Does this ever occur ???
      = pprPanic "SCCfinal: CAF with cc:" (ppr cc)

    do_top_rhs binder (StgRhsClosure no_ccs bi fv u srt args body)
	-- Top level function, probably subsumed
      | noCCSAttached no_ccs
      = set_lambda_cc (do_expr body)	`thenMM` \ body' ->
	returnMM (StgRhsClosure subsumedCCS bi fv u srt args body')

      | otherwise
      = pprPanic "SCCfinal: CAF with cc:" (ppr no_ccs)

    do_top_rhs binder (StgRhsCon ccs con args)
	-- Top-level (static) data is not counted in heap
	-- profiles; nor do we set CCCS from it; so we
	-- just slam in dontCareCostCentre
      = returnMM (StgRhsCon dontCareCCS con args)

    ------
    do_expr :: StgExpr -> MassageM StgExpr

    do_expr (StgLit l) = returnMM (StgLit l)

    do_expr (StgApp fn args)
      = boxHigherOrderArgs (StgApp fn) args

    do_expr (StgConApp con args)
      = boxHigherOrderArgs (\args -> StgConApp con args) args

    do_expr (StgOpApp con args res_ty)
      = boxHigherOrderArgs (\args -> StgOpApp con args res_ty) args

    do_expr (StgSCC cc expr)	-- Ha, we found a cost centre!
      = collectCC cc		`thenMM_`
	do_expr expr		`thenMM` \ expr' ->
	returnMM (StgSCC cc expr')

    do_expr (StgCase expr fv1 fv2 bndr srt alt_type alts)
      = do_expr expr		`thenMM` \ expr' ->
	mapMM do_alt alts 	`thenMM` \ alts' ->
	returnMM (StgCase expr' fv1 fv2 bndr srt alt_type alts')
      where
	do_alt (id, bs, use_mask, e)
	  = do_expr e `thenMM` \ e' ->
	    returnMM (id, bs, use_mask, e')

    do_expr (StgLet b e)
	= do_let b e `thenMM` \ (b,e) ->
	  returnMM (StgLet b e)

    do_expr (StgLetNoEscape lvs1 lvs2 b e)
	= do_let b e `thenMM` \ (b,e) ->
	  returnMM (StgLetNoEscape lvs1 lvs2 b e)

#ifdef DEBUG
    do_expr other = pprPanic "SCCfinal.do_expr" (ppr other)
#endif

    ----------------------------------

    do_let (StgNonRec b rhs) e
      = do_rhs rhs		 	`thenMM` \ rhs' ->
	addTopLevelIshId b (
	  do_expr e		  	`thenMM` \ e' ->
	  returnMM (StgNonRec b rhs',e')
        )

    do_let (StgRec pairs) e
      = addTopLevelIshIds binders (
	   mapMM do_pair pairs	 	`thenMM` \ pairs' ->
	   do_expr e		  	`thenMM` \ e' ->
	   returnMM (StgRec pairs', e')
	)
      where
	binders = map fst pairs
	do_pair (b, rhs) 
	   = do_rhs rhs			`thenMM` \ rhs2 ->
	     returnMM (b, rhs2)

    ----------------------------------
    do_rhs :: StgRhs -> MassageM StgRhs
	-- We play much the same game as we did in do_top_rhs above;
	-- but we don't have to worry about cafs etc.

{-
    do_rhs (StgRhsClosure closure_cc bi fv u [] (StgSCC ty cc (StgCon (DataCon con) args _)))
      | not (isSccCountCostCentre cc)
      = collectCC cc `thenMM_`
	returnMM (StgRhsCon cc con args)
-}

    do_rhs (StgRhsClosure _ bi fv u srt args expr)
      = slurpSCCs currentCCS expr		`thenMM` \ (expr', ccs) ->
	do_expr expr'				`thenMM` \ expr'' ->
	returnMM (StgRhsClosure ccs bi fv u srt args expr'')
      where
	slurpSCCs ccs (StgSCC cc e) 
	     = collectCC cc 			`thenMM_`
	       slurpSCCs ccs e			`thenMM` \ (e', ccs')  ->
	       returnMM (e', pushCCOnCCS cc ccs')
	slurpSCCs ccs e 
	     = returnMM (e, ccs)

    do_rhs (StgRhsCon cc con args)
      = returnMM (StgRhsCon currentCCS con args)
\end{code}

%************************************************************************
%*									*
\subsection{Boxing higher-order args}
%*									*
%************************************************************************

Boxing is *turned off* at the moment, until we can figure out how to
do it properly in general.

\begin{code}
boxHigherOrderArgs
    :: ([StgArg] -> StgExpr)
			-- An application lacking its arguments
    -> [StgArg]		-- arguments which we might box
    -> MassageM StgExpr

#ifndef PROF_DO_BOXING
boxHigherOrderArgs almost_expr args
   = returnMM (almost_expr args)
#else
boxHigherOrderArgs almost_expr args
  = getTopLevelIshIds		`thenMM` \ ids ->
    mapAccumMM (do_arg ids) [] args	`thenMM` \ (let_bindings, new_args) ->
    returnMM (foldr (mk_stg_let currentCCS) (almost_expr new_args) let_bindings)
  where
    ---------------

    do_arg ids bindings arg@(StgVarArg old_var)
	|  (not (isLocalVar old_var) || elemVarSet old_var ids)
	&& isFunTy (dropForAlls var_type)
      =     -- make a trivial let-binding for the top-level function
	getUniqueMM		`thenMM` \ uniq ->
	let
	    new_var = mkSysLocal FSLIT("sf") uniq var_type
	in
	returnMM ( (new_var, old_var) : bindings, StgVarArg new_var )
      where
	var_type = idType old_var

    do_arg ids bindings arg = returnMM (bindings, arg)

    ---------------
    mk_stg_let :: CostCentreStack -> (Id, Id) -> StgExpr -> StgExpr

    mk_stg_let cc (new_var, old_var) body
      = let
	    rhs_body    = StgApp old_var [{-args-}]
	    rhs_closure = StgRhsClosure cc stgArgOcc [{-fvs-}] ReEntrant NoSRT{-eeek!!!-} [{-args-}] rhs_body
        in
	StgLet (StgNonRec new_var rhs_closure) body
      where
	bOGUS_LVs = emptyUniqSet -- easier to print than: panic "mk_stg_let: LVs"
#endif
\end{code}

%************************************************************************
%*									*
\subsection{Boring monad stuff for this}
%*									*
%************************************************************************

\begin{code}
type MassageM result
  =  Module		-- module name
  -> CostCentreStack	-- prevailing CostCentre
			-- if none, subsumedCosts at top-level
			-- currentCostCentre at nested levels
  -> UniqSupply
  -> VarSet		-- toplevel-ish Ids for boxing
  -> CollectedCCs
  -> (CollectedCCs, result)

-- the initMM function also returns the final CollectedCCs

initMM :: Module	-- module name, which we may consult
       -> UniqSupply
       -> MassageM a
       -> (CollectedCCs, a)

initMM mod_name init_us m = m mod_name noCCS init_us emptyVarSet ([],[],[])

thenMM  :: MassageM a -> (a -> MassageM b) -> MassageM b
thenMM_ :: MassageM a -> (MassageM b) -> MassageM b

thenMM expr cont mod scope_cc us ids ccs
  = case splitUniqSupply us	of { (s1, s2) ->
    case (expr mod scope_cc s1 ids ccs) of { (ccs2, result) ->
    cont result mod scope_cc s2 ids ccs2 }}

thenMM_ expr cont mod scope_cc us ids ccs
  = case splitUniqSupply us	of { (s1, s2) ->
    case (expr mod scope_cc s1 ids ccs)	of { (ccs2, _) ->
    cont mod scope_cc s2 ids ccs2 }}

returnMM :: a -> MassageM a
returnMM result mod scope_cc us ids ccs = (ccs, result)

nopMM :: MassageM ()
nopMM mod scope_cc us ids ccs = (ccs, ())

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
getUniqueMM mod scope_cc us ids ccs = (ccs, uniqFromSupply us)

addTopLevelIshId :: Id -> MassageM a -> MassageM a
addTopLevelIshId id scope mod scope_cc us ids ccs
  | isCurrentCCS scope_cc = scope mod scope_cc us ids ccs
  | otherwise             = scope mod scope_cc us (extendVarSet ids id) ccs

addTopLevelIshIds :: [Id] -> MassageM a -> MassageM a
addTopLevelIshIds [] cont = cont
addTopLevelIshIds (id:ids) cont 
  = addTopLevelIshId id (addTopLevelIshIds ids cont)

getTopLevelIshIds :: MassageM VarSet
getTopLevelIshIds mod scope_cc us ids ccs = (ccs, ids)
\end{code}

The prevailing CCS is used to tell whether we're in a top-levelish
position, where top-levelish is defined as "not inside a lambda".
Prevailing CCs used to be used for something much more complicated,
I'm sure --SDM

\begin{code}
set_lambda_cc :: MassageM a -> MassageM a
set_lambda_cc action mod scope_cc us ids ccs
  = action mod currentCCS us ids ccs

set_prevailing_cc :: CostCentreStack -> MassageM a -> MassageM a
set_prevailing_cc cc_to_set_to action mod scope_cc us ids ccs
  = action mod cc_to_set_to us ids ccs

get_prevailing_cc :: MassageM CostCentreStack
get_prevailing_cc mod scope_cc us ids ccs = (ccs, scope_cc)
\end{code}

\begin{code}
collectCC :: CostCentre -> MassageM ()

collectCC cc mod_name scope_cc us ids (local_ccs, extern_ccs, ccss)
  = ASSERT(not (noCCAttached cc))
    if (cc `ccFromThisModule` mod_name) then
	((cc : local_ccs, extern_ccs, ccss), ())
    else -- must declare it "extern"
	((local_ccs, cc : extern_ccs, ccss), ())

collectCCS :: CostCentreStack -> MassageM ()

collectCCS ccs mod_name scope_cc us ids (local_ccs, extern_ccs, ccss)
  = ASSERT(not (noCCSAttached ccs))
    ((local_ccs, extern_ccs, ccs : ccss), ())
\end{code}
