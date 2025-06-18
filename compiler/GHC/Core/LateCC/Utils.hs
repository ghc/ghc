module GHC.Core.LateCC.Utils
  ( -- * Inserting cost centres
    doLateCostCenters -- Might be useful for API users

    -- ** Helpers for defining insertion methods
  , getCCFlavour
  , insertCC
  , insertCCRhs
  ) where

import GHC.Prelude

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Set as S

import GHC.Core
import GHC.Core.LateCC.Types
import GHC.Core.Utils
import GHC.Data.FastString
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.SrcLoc
import GHC.Types.Tickish

-- | Insert cost centres into the 'CoreProgram' using the provided environment,
-- initial state, and insertion method.
doLateCostCenters
  :: LateCCEnv
  -- ^ Environment to run the insertion in
  -> LateCCState s
  -- ^ Initial state to run the insertion with
  -> (CoreBind -> LateCCM s CoreBind)
  -- ^ Insertion method
  -> CoreProgram
  -- ^ Bindings to consider
  -> (CoreProgram, LateCCState s)
doLateCostCenters env state method binds =
    runLateCC env state $ mapM method binds

-- | Evaluate late cost centre insertion
runLateCC :: LateCCEnv -> LateCCState s -> LateCCM s a -> (a, LateCCState s)
runLateCC env state = (`runState` state) . (`runReaderT` env)

-- | Given the name of a cost centre, get its flavour
getCCFlavour :: FastString -> LateCCM s CCFlavour
getCCFlavour name = mkLateCCFlavour <$> getCCIndex' name
  where
    getCCIndex' :: FastString -> LateCCM s CostCentreIndex
    getCCIndex' name = do
      cc_state <- lift $ gets lateCCState_ccState
      let (index, cc_state') = getCCIndex name cc_state
      lift . modify $ \s -> s { lateCCState_ccState = cc_state'}
      return index

-- | Insert a cost centre with the specified name and source span on the given
-- expression. The inserted cost centre will be appropriately tracked in the
-- late cost centre state.
insertCC
  :: FastString
  -- ^ Name of the cost centre to insert
  -> SrcSpan
  -- ^ Source location to associate with the cost centre
  -> CoreExpr
  -- ^ Expression to wrap in the cost centre
  -> LateCCM s CoreExpr
insertCC cc_name cc_loc expr = do
    cc_flavour <- getCCFlavour cc_name
    env <- ask
    let
      cc_mod = lateCCEnv_module env
      cc = NormalCC cc_flavour cc_name cc_mod cc_loc
      note = ProfNote cc (lateCCEnv_countEntries env) True
    when (lateCCEnv_collectCCs env) $ do
        lift . modify $ \s ->
          s { lateCCState_ccs = S.insert cc (lateCCState_ccs s)
            }
    return $ mkTick note expr

-- | Insert a cost centre with the specified name and source span on the rhs
-- of a binder. The inserted cost centre will be appropriately tracked in the
-- late cost centre state.
insertCCRhs
  :: FastString
  -- ^ Name of the cost centre to insert
  -> SrcSpan
  -- ^ Source location to associate with the cost centre
  -> CoreExpr
  -- ^ Expression to wrap in the cost centre
  -> LateCCM s CoreExpr
insertCCRhs cc_name cc_loc expr = addCC expr
  where
    -- For binder rhss we want to put the cost centre below the lambda as we only care about
    -- executions of the RHS. Note that the lambdas might be hidden under ticks
    -- or casts. So look through these as well.
    addCC :: CoreExpr -> LateCCM s CoreExpr
    addCC (Cast rhs co) = pure Cast <*> addCC rhs <*> pure co
    addCC (Tick t rhs) = (Tick t) <$> addCC rhs
    addCC (Lam b rhs) = Lam b <$> addCC rhs
    addCC rhs = do
      insertCC cc_name cc_loc rhs

