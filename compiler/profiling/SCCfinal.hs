-- (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- Modify and collect code generation for final STG program

{-
 This is now a sort-of-normal STG-to-STG pass (WDP 94/06), run by stg2stg.

  - Traverses the STG program collecting the cost centres. These are required
    to declare the cost centres at the start of code generation.

    Note: because of cross-module unfolding, some of these cost centres may be
    from other modules.

  - Puts on CAF cost-centres if the user has asked for individual CAF
    cost-centres.
-}

module SCCfinal ( stgMassageForProfiling ) where

#include "HsVersions.h"

import GhcPrelude

import StgSyn

import CostCentre       -- lots of things
import Id
import Name
import Module
import UniqSupply       ( UniqSupply )
import Outputable
import DynFlags
import CoreSyn          ( Tickish(..) )
import FastString
import SrcLoc
import Util

import Control.Monad (liftM, ap)

stgMassageForProfiling
        :: DynFlags
        -> Module                       -- module name
        -> UniqSupply                   -- unique supply
        -> [StgTopBinding]              -- input
        -> (CollectedCCs, [StgTopBinding])

stgMassageForProfiling dflags mod_name _us stg_binds
  = let
        ((local_ccs, cc_stacks),
         stg_binds2)
          = initMM mod_name (do_top_bindings stg_binds)

        (fixed_ccs, fixed_cc_stacks)
          = if gopt Opt_AutoSccsOnIndividualCafs dflags
            then ([],[])  -- don't need "all CAFs" CC
            else ([all_cafs_cc], [all_cafs_ccs])

        local_ccs_no_dups  = nubSort local_ccs
    in
    ((fixed_ccs ++ local_ccs_no_dups,
      fixed_cc_stacks ++ cc_stacks), stg_binds2)
  where

    span = mkGeneralSrcSpan (mkFastString "<entire-module>") -- XXX do better
    all_cafs_cc  = mkAllCafsCC mod_name span
    all_cafs_ccs = mkSingletonCCS all_cafs_cc

    ----------
    do_top_bindings :: [StgTopBinding] -> MassageM [StgTopBinding]

    do_top_bindings [] = return []

    do_top_bindings (StgTopLifted (StgNonRec b rhs) : bs) = do
        rhs' <- do_top_rhs b rhs
        bs' <- do_top_bindings bs
        return (StgTopLifted (StgNonRec b rhs') : bs')

    do_top_bindings (StgTopLifted (StgRec pairs) : bs) = do
        pairs2 <- mapM do_pair pairs
        bs' <- do_top_bindings bs
        return (StgTopLifted (StgRec pairs2) : bs')
      where
        do_pair (b, rhs) = do
             rhs2 <- do_top_rhs b rhs
             return (b, rhs2)

    do_top_bindings (b@StgTopStringLit{} : bs) = do
        bs' <- do_top_bindings bs
        return (b : bs')

    ----------
    do_top_rhs :: Id -> StgRhs -> MassageM StgRhs

    do_top_rhs _ (StgRhsClosure _ _ _ _ []
                     (StgTick (ProfNote _cc False{-not tick-} _push)
                              (StgConApp con args _)))
      | not (isDllConApp dflags mod_name con args)
        -- Trivial _scc_ around nothing but static data
        -- Eliminate _scc_ ... and turn into StgRhsCon

        -- isDllConApp checks for LitLit args too
      = return (StgRhsCon dontCareCCS con args)

    do_top_rhs binder (StgRhsClosure _ bi fv u [] body)
      = do
        -- Top level CAF without a cost centre attached
        -- Attach CAF cc (collect if individual CAF ccs)
        caf_ccs <- if gopt Opt_AutoSccsOnIndividualCafs dflags
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
        body' <- do_expr body
        return (StgRhsClosure caf_ccs bi fv u [] body')

    do_top_rhs _ (StgRhsClosure _no_ccs bi fv u args body)
      = do body' <- do_expr body
           return (StgRhsClosure dontCareCCS bi fv u args body')

    do_top_rhs _ (StgRhsCon _ con args)
        -- Top-level (static) data is not counted in heap
        -- profiles; nor do we set CCCS from it; so we
        -- just slam in dontCareCostCentre
      = return (StgRhsCon dontCareCCS con args)

    ------
    do_expr :: StgExpr -> MassageM StgExpr

    do_expr (StgLit l) = return (StgLit l)

    do_expr (StgApp fn args)
      = return (StgApp fn args)

    do_expr (StgConApp con args ty_args)
      = return (StgConApp con args ty_args)

    do_expr (StgOpApp con args res_ty)
      = return (StgOpApp con args res_ty)

    do_expr (StgTick note@(ProfNote cc _ _) expr) = do
        -- Ha, we found a cost centre!
        collectCC cc
        expr' <- do_expr expr
        return (StgTick note expr')

    do_expr (StgTick ti expr) = do
        expr' <- do_expr expr
        return (StgTick ti expr')

    do_expr (StgCase expr bndr alt_type alts) = do
        expr' <- do_expr expr
        alts' <- mapM do_alt alts
        return (StgCase expr' bndr alt_type alts')
      where
        do_alt (id, bs, e) = do
            e' <- do_expr e
            return (id, bs, e')

    do_expr (StgLet b e) = do
          (b,e) <- do_let b e
          return (StgLet b e)

    do_expr (StgLetNoEscape b e) = do
          (b,e) <- do_let b e
          return (StgLetNoEscape b e)

    do_expr other = pprPanic "SCCfinal.do_expr" (ppr other)

    ----------------------------------

    do_let (StgNonRec b rhs) e = do
        rhs' <- do_rhs rhs
        e' <- do_expr e
        return (StgNonRec b rhs',e')

    do_let (StgRec pairs) e = do
        pairs' <- mapM do_pair pairs
        e' <- do_expr e
        return (StgRec pairs', e')
      where
        do_pair (b, rhs) = do
             rhs2 <- do_rhs rhs
             return (b, rhs2)

    ----------------------------------
    do_rhs :: StgRhs -> MassageM StgRhs
        -- We play much the same game as we did in do_top_rhs above;
        -- but we don't have to worry about cafs etc.

        -- throw away the SCC if we don't have to count entries.  This
        -- is a little bit wrong, because we're attributing the
        -- allocation of the constructor to the wrong place (XXX)
        -- We should really attach (PushCC cc CurrentCCS) to the rhs,
        -- but need to reinstate PushCC for that.
    do_rhs (StgRhsClosure _closure_cc _bi _fv _u []
               (StgTick (ProfNote cc False{-not tick-} _push)
                        (StgConApp con args _)))
      = do collectCC cc
           return (StgRhsCon currentCCS con args)

    do_rhs (StgRhsClosure _ bi fv u args expr) = do
        expr' <- do_expr expr
        return (StgRhsClosure currentCCS bi fv u args expr')

    do_rhs (StgRhsCon _ con args)
      = return (StgRhsCon currentCCS con args)


-- -----------------------------------------------------------------------------
-- Boring monad stuff for this

newtype MassageM result
  = MassageM {
      unMassageM :: Module              -- module name
                 -> CollectedCCs
                 -> (CollectedCCs, result)
    }

instance Functor MassageM where
      fmap = liftM

instance Applicative MassageM where
      pure x = MassageM (\_ ccs -> (ccs, x))
      (<*>) = ap
      (*>) = thenMM_

instance Monad MassageM where
    (>>=) = thenMM
    (>>)  = (*>)

-- the initMM function also returns the final CollectedCCs

initMM :: Module        -- module name, which we may consult
       -> MassageM a
       -> (CollectedCCs, a)

initMM mod_name (MassageM m) = m mod_name ([],[])

thenMM  :: MassageM a -> (a -> MassageM b) -> MassageM b
thenMM_ :: MassageM a -> (MassageM b) -> MassageM b

thenMM expr cont = MassageM $ \mod ccs ->
    case unMassageM expr mod ccs of { (ccs2, result) ->
    unMassageM (cont result) mod ccs2 }

thenMM_ expr cont = MassageM $ \mod ccs ->
    case unMassageM expr mod ccs of { (ccs2, _) ->
    unMassageM cont mod ccs2 }


collectCC :: CostCentre -> MassageM ()
collectCC cc
 = MassageM $ \mod_name (local_ccs, ccss)
  -> if (cc `ccFromThisModule` mod_name) then
        ((cc : local_ccs, ccss), ())
     else
        ((local_ccs, ccss), ())

-- Version of collectCC used when we definitely want to declare this
-- CC as local, even if its module name is not the same as the current
-- module name (eg. the special :Main module) see bug #249, #1472,
-- test prof001,prof002.
collectNewCC :: CostCentre -> MassageM ()
collectNewCC cc
 = MassageM $ \_mod_name (local_ccs, ccss)
              -> ((cc : local_ccs, ccss), ())

collectCCS :: CostCentreStack -> MassageM ()

collectCCS ccs
 = MassageM $ \_mod_name (local_ccs, ccss)
              -> ASSERT(not (noCCSAttached ccs))
                       ((local_ccs, ccs : ccss), ())
