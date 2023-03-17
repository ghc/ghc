{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Adds cost-centers after the core piple has run.
module GHC.Core.LateCC
    ( addLateCostCentresMG
    , addLateCostCentresPgm
    , addLateCostCentres -- Might be useful for API users
    , Env(..)
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Set as S

import GHC.Prelude
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.Name hiding (varName)
import GHC.Types.Tickish
import GHC.Unit.Module.ModGuts
import GHC.Types.Var
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Core
import GHC.Core.Opt.Monad
import GHC.Core.Utils (mkTick)
import GHC.Types.Id
import GHC.Driver.Session

import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Error (withTiming)
import GHC.Utils.Monad.State.Strict


{- Note [Collecting late cost centres]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usually cost centres defined by a module are collected
during tidy by collectCostCentres. However with `-fprof-late`
we insert cost centres after inlining. So we keep a list of
all the cost centres we inserted and combine that with the list
of cost centres found during tidy.

To avoid overhead when using -fprof-inline there is a flag to stop
us from collecting them here when we run this pass before tidy.

Note [Adding late cost centres]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

addLateCostCentresMG :: ModGuts -> CoreM ModGuts
addLateCostCentresMG guts = do
  dflags <- getDynFlags
  let env :: Env
      env = Env
        { thisModule = mg_module guts
        , ccState = newCostCentreState
        , countEntries = gopt Opt_ProfCountEntries dflags
        , collectCCs = False -- See Note [Collecting late cost centres]
        }
  let guts' = guts { mg_binds = fst (addLateCostCentres env (mg_binds guts))
                   }
  return guts'

addLateCostCentresPgm :: DynFlags -> Logger -> Module -> CoreProgram -> IO (CoreProgram, S.Set CostCentre)
addLateCostCentresPgm dflags logger mod binds =
  withTiming logger
               (text "LateCC"<+>brackets (ppr mod))
               (\(a,b) -> a `seqList` (b `seq` ())) $ do
  let env = Env
        { thisModule = mod
        , ccState = newCostCentreState
        , countEntries = gopt Opt_ProfCountEntries dflags
        , collectCCs = True -- See Note [Collecting late cost centres]
        }
      (binds', ccs) = addLateCostCentres env binds
  when (dopt Opt_D_dump_late_cc dflags || dopt Opt_D_verbose_core2core dflags) $
    putDumpFileMaybe logger Opt_D_dump_late_cc "LateCC" FormatCore (vcat (map ppr binds'))
  return (binds', ccs)

addLateCostCentres :: Env -> CoreProgram -> (CoreProgram,S.Set CostCentre)
addLateCostCentres env binds =
  let (binds', state) = runState (mapM (doBind env) binds) initLateCCState
  in (binds',lcs_ccs state)


doBind :: Env -> CoreBind -> M CoreBind
doBind env (NonRec b rhs) = NonRec b <$> doBndr env b rhs
doBind env (Rec bs) = Rec <$> mapM doPair bs
  where
    doPair :: ((Id, CoreExpr) -> M (Id, CoreExpr))
    doPair (b,rhs) = (b,) <$> doBndr env b rhs

doBndr :: Env -> Id -> CoreExpr -> M CoreExpr
doBndr env bndr rhs
  -- Cost centres on constructor workers are pretty much useless
  -- so we don't emit them if we are looking at the rhs of a constructor
  -- binding.
  | Just _ <- isDataConId_maybe bndr = pure rhs
  | otherwise = doBndr' env bndr rhs


-- We want to put the cost centre below the lambda as we only care about executions of the RHS.
doBndr' :: Env -> Id -> CoreExpr -> State LateCCState CoreExpr
doBndr' env bndr (Lam b rhs) = Lam b <$> doBndr' env bndr rhs
doBndr' env bndr rhs = do
    let name = idName bndr
        name_loc = nameSrcSpan name
        cc_name = getOccFS name
        count = countEntries env
    cc_flavour <- getCCFlavour cc_name
    let cc_mod = thisModule env
        bndrCC = NormalCC cc_flavour cc_name cc_mod name_loc
        note = ProfNote bndrCC count True
    addCC env bndrCC
    return $ mkTick note rhs

data LateCCState = LateCCState
    { lcs_state :: !CostCentreState
    , lcs_ccs   :: S.Set CostCentre
    }
type M = State LateCCState

initLateCCState :: LateCCState
initLateCCState = LateCCState newCostCentreState mempty

getCCFlavour :: FastString -> M CCFlavour
getCCFlavour name = mkLateCCFlavour <$> getCCIndex' name

getCCIndex' :: FastString -> M CostCentreIndex
getCCIndex' name = do
  state <- get
  let (index,cc_state') = getCCIndex name (lcs_state state)
  put (state { lcs_state = cc_state'})
  return index

addCC :: Env -> CostCentre -> M ()
addCC !env cc = do
    state <- get
    when (collectCCs env) $ do
        let ccs' = S.insert cc (lcs_ccs state)
        put (state { lcs_ccs = ccs'})

data Env = Env
  { thisModule  :: !Module
  , countEntries:: !Bool
  , ccState     :: !CostCentreState
  , collectCCs  :: !Bool
  }

