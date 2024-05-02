{-# LANGUAGE TupleSections #-}
module GHC.Core.LateCC.TopLevelBinds where

import GHC.Prelude

import GHC.Core
-- import GHC.Core.LateCC
import GHC.Core.LateCC.Types
import GHC.Core.LateCC.Utils
import GHC.Core.Opt.Monad
import GHC.Driver.DynFlags
import GHC.Types.Id
import GHC.Types.Name
import GHC.Unit.Module.ModGuts

{- Note [Collecting late cost centres]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usually cost centres defined by a module are collected
during tidy by collectCostCentres. However with `-fprof-late`
we insert cost centres after inlining. So we keep a list of
all the cost centres we inserted and combine that with the list
of cost centres found during tidy.

To avoid overhead when using -fprof-inline there is a flag to stop
us from collecting them here when we run this pass before tidy.

Note [Adding late cost centres to top level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea is very simple. For every top level binder
`f = rhs` we compile it as if the user had written
`f = {-# SCC f #-} rhs`.

If we do this after unfoldings for `f` have been created this
doesn't impact core-level optimizations at all. If we do it
before the cost centre will be included in the unfolding and
might inhibit optimizations at the call site. For this reason
we provide flags for both approaches as they have different
tradeoffs.

We also don't add a cost centre for any binder that is a constructor
worker or wrapper. These will never meaningfully enrich the resulting
profile so we improve efficiency by omitting those.

-}

-- | Add late cost centres directly to the 'ModGuts'. This is used inside the
-- core pipeline with the -fprof-late-inline flag. It should not be used after
-- tidy, since it does not manually track inserted cost centers. See
-- Note [Collecting late cost centres].
topLevelBindsCCMG :: ModGuts -> CoreM ModGuts
topLevelBindsCCMG guts = do
    dflags <- getDynFlags
    let
      env =
        LateCCEnv
          { lateCCEnv_module = mg_module guts

            -- We don't use this for topLevelBindsCC, so Nothing is okay
          , lateCCEnv_file = Nothing

          , lateCCEnv_countEntries= gopt Opt_ProfCountEntries dflags
          , lateCCEnv_collectCCs = False
          }
      guts' =
        guts
          { mg_binds =
              fst
                ( doLateCostCenters
                    env
                    (initLateCCState ())
                    (topLevelBindsCC (const True))
                    (mg_binds guts)
                )
          }
    return guts'

-- | Insert cost centres on top-level bindings in the module, depending on
-- whether or not they satisfy the given predicate.
topLevelBindsCC :: (CoreExpr -> Bool) -> CoreBind -> LateCCM s CoreBind
topLevelBindsCC pred core_bind =
    case core_bind of
      NonRec b rhs ->
        NonRec b <$> doBndr b rhs
      Rec bs ->
        Rec <$> mapM doPair bs
  where
    doPair :: ((Id, CoreExpr) -> LateCCM s (Id, CoreExpr))
    doPair (b,rhs) = (b,) <$> doBndr b rhs

    doBndr :: Id -> CoreExpr -> LateCCM s CoreExpr
    doBndr bndr rhs
      -- Cost centres on constructor workers are pretty much useless
      -- so we don't emit them if we are looking at the rhs of a constructor
      -- binding.
      | Just _ <- isDataConId_maybe bndr = pure rhs
      | otherwise = if pred rhs then addCC bndr rhs else pure rhs

    -- We want to put the cost centre below the lambda as we only care about
    -- executions of the RHS.
    addCC :: Id -> CoreExpr -> LateCCM s CoreExpr
    addCC bndr (Cast rhs co) = pure Cast <*> addCC bndr rhs <*> pure co
    addCC bndr (Tick t rhs) = (Tick t) <$> addCC bndr rhs
    addCC bndr (Lam b rhs) = Lam b <$> addCC bndr rhs
    addCC bndr rhs = do
      let name = idName bndr
          cc_loc = nameSrcSpan name
          cc_name = getOccFS name
      insertCC cc_name cc_loc rhs