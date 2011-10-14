%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SCCfinal]{Modify and collect code generation for final STG program}

This is now a sort-of-normal STG-to-STG pass (WDP 94/06), run by stg2stg.

 - Traverses the STG program collecting the cost centres. These are required
   to declare the cost centres at the start of code generation.

   Note: because of cross-module unfolding, some of these cost centres may be
   from other modules.  But will still have to give them "extern"
   declarations.

 - Puts on CAF cost-centres if the user has asked for individual CAF
   cost-centres.

 - Ditto for individual DICT cost-centres.

 - Boxes top-level inherited functions passed as arguments.

 - "Distributes" given cost-centres to all as-yet-unmarked RHSs.

\begin{code}
module SCCfinal ( stgMassageForProfiling ) where

#include "HsVersions.h"

import StgSyn

import CostCentre       -- lots of things
import Id
import Name
import Module
import UniqSupply       ( splitUniqSupply, UniqSupply )
#ifdef PROF_DO_BOXING
import UniqSupply       ( uniqFromSupply )
#endif
import VarSet
import ListSetOps       ( removeDups )
import Outputable
import DynFlags
\end{code}

\begin{code}
stgMassageForProfiling
        :: DynFlags
        -> Module                       -- module name
        -> UniqSupply                   -- unique supply
        -> [StgBinding]                 -- input
        -> (CollectedCCs, [StgBinding])

stgMassageForProfiling dflags mod_name us stg_binds
  = let
        ((local_ccs, extern_ccs, cc_stacks),
         stg_binds2)
          = initMM mod_name us (do_top_bindings stg_binds)

        (fixed_ccs, fixed_cc_stacks)
          = if dopt Opt_AutoSccsOnIndividualCafs dflags
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

    do_top_bindings [] = return []

    do_top_bindings (StgNonRec b rhs : bs) = do
        rhs' <- do_top_rhs b rhs
        addTopLevelIshId b $ do
           bs' <- do_top_bindings bs
           return (StgNonRec b rhs' : bs')

    do_top_bindings (StgRec pairs : bs)
      = addTopLevelIshIds binders $ do
           pairs2 <- mapM do_pair pairs
           bs' <- do_top_bindings bs
           return (StgRec pairs2 : bs')
      where
        binders = map fst pairs
        do_pair (b, rhs) = do
             rhs2 <- do_top_rhs b rhs
             return (b, rhs2)

    ----------
    do_top_rhs :: Id -> StgRhs -> MassageM StgRhs

    do_top_rhs _ (StgRhsClosure _ _ _ _ _ [] (StgSCC cc (StgConApp con args)))
      | not (isSccCountCostCentre cc) && not (isDllConApp dflags con args)
        -- Trivial _scc_ around nothing but static data
        -- Eliminate _scc_ ... and turn into StgRhsCon

        -- isDllConApp checks for LitLit args too
      = return (StgRhsCon dontCareCCS con args)

{- Can't do this one with cost-centre stacks:  --SDM
    do_top_rhs binder (StgRhsClosure no_cc bi fv u [] (StgSCC ty cc expr))
      | (noCCSAttached no_cc || currentOrSubsumedCCS no_cc)
        && not (isSccCountCostCentre cc)
        -- Top level CAF without a cost centre attached
        -- Attach and collect cc of trivial _scc_ in body
      = do collectCC cc
           expr' <- set_prevailing_cc cc (do_expr expr)
           return (StgRhsClosure cc bi fv u [] expr')
-}

    do_top_rhs binder (StgRhsClosure no_cc bi fv u srt [] body)
      | noCCSAttached no_cc || currentOrSubsumedCCS no_cc = do
        -- Top level CAF without a cost centre attached
        -- Attach CAF cc (collect if individual CAF ccs)
        caf_ccs <- if dopt Opt_AutoSccsOnIndividualCafs dflags
                   then let cc = mkAutoCC binder modl CafCC
                            ccs = mkSingletonCCS cc
                                   -- careful: the binder might be :Main.main,
                                   -- which doesn't belong to module mod_name.
                                   -- bug #249, tests prof001, prof002
                            modl | Just m <- nameModule_maybe (idName binder) = m
                                 | otherwise = mod_name
                        in do
                        collectNewCC  cc
                        collectCCS ccs
                        return ccs
                   else
                        return all_cafs_ccs
        body' <- set_prevailing_cc caf_ccs (do_expr body)
        return (StgRhsClosure caf_ccs bi fv u srt [] body')

    do_top_rhs _ (StgRhsClosure cc _ _ _ _ [] _)
        -- Top level CAF with cost centre attached
        -- Should this be a CAF cc ??? Does this ever occur ???
      = pprPanic "SCCfinal: CAF with cc:" (ppr cc)

    do_top_rhs _ (StgRhsClosure no_ccs bi fv u srt args body)
        -- Top level function, probably subsumed
      | noCCSAttached no_ccs
      = do body' <- set_lambda_cc (do_expr body)
           return (StgRhsClosure subsumedCCS bi fv u srt args body')

      | otherwise
      = pprPanic "SCCfinal: CAF with cc:" (ppr no_ccs)

    do_top_rhs _ (StgRhsCon _ con args)
        -- Top-level (static) data is not counted in heap
        -- profiles; nor do we set CCCS from it; so we
        -- just slam in dontCareCostCentre
      = return (StgRhsCon dontCareCCS con args)

    ------
    do_expr :: StgExpr -> MassageM StgExpr

    do_expr (StgLit l) = return (StgLit l)

    do_expr (StgApp fn args)
      = boxHigherOrderArgs (StgApp fn) args

    do_expr (StgConApp con args)
      = boxHigherOrderArgs (\args -> StgConApp con args) args

    do_expr (StgOpApp con args res_ty)
      = boxHigherOrderArgs (\args -> StgOpApp con args res_ty) args

    do_expr (StgSCC cc expr) = do -- Ha, we found a cost centre!
        collectCC cc
        expr' <- do_expr expr
        return (StgSCC cc expr')

    do_expr (StgCase expr fv1 fv2 bndr srt alt_type alts) = do
        expr' <- do_expr expr
        alts' <- mapM do_alt alts
        return (StgCase expr' fv1 fv2 bndr srt alt_type alts')
      where
        do_alt (id, bs, use_mask, e) = do
            e' <- do_expr e
            return (id, bs, use_mask, e')

    do_expr (StgLet b e) = do
          (b,e) <- do_let b e
          return (StgLet b e)

    do_expr (StgLetNoEscape lvs1 lvs2 b e) = do
          (b,e) <- do_let b e
          return (StgLetNoEscape lvs1 lvs2 b e)

    do_expr (StgTick m n expr) = do
          expr' <- do_expr expr
          return (StgTick m n expr')

    do_expr other = pprPanic "SCCfinal.do_expr" (ppr other)

    ----------------------------------

    do_let (StgNonRec b rhs) e = do
        rhs' <- do_rhs rhs
        addTopLevelIshId b $ do
          e' <- do_expr e
          return (StgNonRec b rhs',e')

    do_let (StgRec pairs) e
      = addTopLevelIshIds binders $ do
           pairs' <- mapM do_pair pairs
           e' <- do_expr e
           return (StgRec pairs', e')
      where
        binders = map fst pairs
        do_pair (b, rhs) = do
             rhs2 <- do_rhs rhs
             return (b, rhs2)

    ----------------------------------
    do_rhs :: StgRhs -> MassageM StgRhs
        -- We play much the same game as we did in do_top_rhs above;
        -- but we don't have to worry about cafs etc.

{-
    do_rhs (StgRhsClosure closure_cc bi fv u [] (StgSCC ty cc (StgCon (DataCon con) args _)))
      | not (isSccCountCostCentre cc)
      = do collectCC cc
           return (StgRhsCon cc con args)
-}

    do_rhs (StgRhsClosure _ bi fv u srt args expr) = do
        (expr', ccs) <- slurpSCCs currentCCS expr
        expr'' <- do_expr expr'
        return (StgRhsClosure ccs bi fv u srt args expr'')
      where
        slurpSCCs ccs (StgSCC cc e)
             = do collectCC cc
                  slurpSCCs (cc `pushCCOnCCS` ccs) e
        slurpSCCs ccs e
             = return (e, ccs)

    do_rhs (StgRhsCon _ con args)
      = return (StgRhsCon currentCCS con args)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Boxing higher-order args}
%*                                                                      *
%************************************************************************

Boxing is *turned off* at the moment, until we can figure out how to
do it properly in general.

\begin{code}
boxHigherOrderArgs
    :: ([StgArg] -> StgExpr)
                        -- An application lacking its arguments
    -> [StgArg]         -- arguments which we might box
    -> MassageM StgExpr

#ifndef PROF_DO_BOXING
boxHigherOrderArgs almost_expr args
   = return (almost_expr args)
#else
boxHigherOrderArgs almost_expr args = do
    ids <- getTopLevelIshIds
    (let_bindings, new_args) <- mapAccumLM (do_arg ids) [] args
    return (foldr (mk_stg_let currentCCS) (almost_expr new_args) let_bindings)
  where
    ---------------

    do_arg ids bindings arg@(StgVarArg old_var)
        |  (not (isLocalVar old_var) || elemVarSet old_var ids)
        && isFunTy (dropForAlls var_type)
      = do    -- make a trivial let-binding for the top-level function
        uniq <- getUniqueMM
        let
            new_var = mkSysLocal (fsLit "sf") uniq var_type
        return ( (new_var, old_var) : bindings, StgVarArg new_var )
      where
        var_type = idType old_var

    do_arg ids bindings arg = return (bindings, arg)

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
%*                                                                      *
\subsection{Boring monad stuff for this}
%*                                                                      *
%************************************************************************

\begin{code}
newtype MassageM result
  = MassageM {
      unMassageM :: Module              -- module name
                 -> CostCentreStack     -- prevailing CostCentre
                                        -- if none, subsumedCosts at top-level
                                        -- currentCostCentre at nested levels
                 -> UniqSupply
                 -> VarSet              -- toplevel-ish Ids for boxing
                 -> CollectedCCs
                 -> (CollectedCCs, result)
    }

instance Monad MassageM where
    return x = MassageM (\_ _ _ _ ccs -> (ccs, x))
    (>>=) = thenMM
    (>>)  = thenMM_

-- the initMM function also returns the final CollectedCCs

initMM :: Module        -- module name, which we may consult
       -> UniqSupply
       -> MassageM a
       -> (CollectedCCs, a)

initMM mod_name init_us (MassageM m) = m mod_name noCCS init_us emptyVarSet ([],[],[])

thenMM  :: MassageM a -> (a -> MassageM b) -> MassageM b
thenMM_ :: MassageM a -> (MassageM b) -> MassageM b

thenMM expr cont = MassageM $ \mod scope_cc us ids ccs ->
    case splitUniqSupply us of { (s1, s2) ->
    case unMassageM expr mod scope_cc s1 ids ccs of { (ccs2, result) ->
    unMassageM (cont result) mod scope_cc s2 ids ccs2 }}

thenMM_ expr cont = MassageM $ \mod scope_cc us ids ccs ->
    case splitUniqSupply us of { (s1, s2) ->
    case unMassageM expr mod scope_cc s1 ids ccs of { (ccs2, _) ->
    unMassageM cont mod scope_cc s2 ids ccs2 }}

#ifdef PROF_DO_BOXING
getUniqueMM :: MassageM Unique
getUniqueMM = MassageM \mod scope_cc us ids ccs -> (ccs, uniqFromSupply us)
#endif

addTopLevelIshId :: Id -> MassageM a -> MassageM a
addTopLevelIshId id scope
   = MassageM $ \mod scope_cc us ids ccs ->
      if isCurrentCCS scope_cc then unMassageM scope mod scope_cc us ids ccs
                               else unMassageM scope mod scope_cc us (extendVarSet ids id) ccs

addTopLevelIshIds :: [Id] -> MassageM a -> MassageM a
addTopLevelIshIds [] cont = cont
addTopLevelIshIds (id:ids) cont
  = addTopLevelIshId id (addTopLevelIshIds ids cont)

#ifdef PROF_DO_BOXING
getTopLevelIshIds :: MassageM VarSet
getTopLevelIshIds = MassageM $ \_mod _scope_cc _us ids ccs -> (ccs, ids)
#endif
\end{code}

The prevailing CCS is used to tell whether we're in a top-levelish
position, where top-levelish is defined as "not inside a lambda".
Prevailing CCs used to be used for something much more complicated,
I'm sure --SDM

\begin{code}
set_lambda_cc :: MassageM a -> MassageM a
set_lambda_cc action
   =    MassageM $     \mod _scope_cc  us ids ccs
   -> unMassageM action mod currentCCS us ids ccs

set_prevailing_cc :: CostCentreStack -> MassageM a -> MassageM a
set_prevailing_cc cc_to_set_to action
   =    MassageM $     \mod _scope_cc    us ids ccs
   -> unMassageM action mod cc_to_set_to us ids ccs
\end{code}

\begin{code}
collectCC :: CostCentre -> MassageM ()
collectCC cc
 = MassageM $ \mod_name _scope_cc _us _ids (local_ccs, extern_ccs, ccss)
  -> ASSERT(not (noCCAttached cc))
     if (cc `ccFromThisModule` mod_name) then
        ((cc : local_ccs, extern_ccs, ccss), ())
     else -- must declare it "extern"
        ((local_ccs, cc : extern_ccs, ccss), ())

-- Version of collectCC used when we definitely want to declare this
-- CC as local, even if its module name is not the same as the current
-- module name (eg. the special :Main module) see bug #249, #1472,
-- test prof001,prof002.
collectNewCC :: CostCentre -> MassageM ()
collectNewCC cc
 = MassageM $ \_mod_name _scope_cc _us _ids (local_ccs, extern_ccs, ccss)
              -> ((cc : local_ccs, extern_ccs, ccss), ())

collectCCS :: CostCentreStack -> MassageM ()

collectCCS ccs
 = MassageM $ \_mod_name _scope_cc _us _ids (local_ccs, extern_ccs, ccss)
              -> ASSERT(not (noCCSAttached ccs))
                       ((local_ccs, extern_ccs, ccs : ccss), ())
\end{code}
