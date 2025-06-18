{-# LANGUAGE RecordWildCards #-}

-- | Adds cost-centers after the core pipline has run.
module GHC.Core.LateCC
    ( -- * Inserting cost centres
      addLateCostCenters
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.LateCC.OverloadedCalls
import GHC.Core.LateCC.TopLevelBinds
import GHC.Core.LateCC.Types
import GHC.Core.LateCC.Utils
import GHC.Core.Lint (lintPassResult)
import GHC.Core.Seq
import qualified GHC.Data.Strict as Strict
import GHC.Core.Utils
import GHC.Tc.Utils.TcType
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Types.RepType (mightBeFunTy)

import Control.Monad (when)
-- | Late cost center insertion logic used by the driver
addLateCostCenters ::
     Logger
  -- ^ Logger
  -> LateCCConfig
  -- ^ Late cost center configuration
  -> CoreProgram
  -- ^ The program
  -> IO (CoreProgram, LateCCState (Strict.Maybe SrcSpan))
addLateCostCenters logger LateCCConfig{..} core_binds = do

    -- If top-level late CCs are enabled via either -fprof-late or
    -- -fprof-late-overloaded, add them
    (top_level_cc_binds, top_level_late_cc_state) <-
      case lateCCConfig_whichBinds of
        LateCCNone ->
          return (core_binds, initLateCCState ())
        _ ->
          withTiming
            logger
            (text "LateTopLevelCCs" <+> brackets (ppr this_mod))
            (\(binds, late_cc_state) -> seqBinds binds `seq` late_cc_state `seq` ())
            $ {-# SCC lateTopLevelCCs #-} do
              pure $
                doLateCostCenters
                  lateCCConfig_env
                  (initLateCCState ())
                  (topLevelBindsCC top_level_cc_pred)
                  core_binds

    -- If overloaded call CCs are enabled via -fprof-late-overloaded-calls, add
    -- them
    (late_cc_binds, late_cc_state) <-
      if lateCCConfig_overloadedCalls then
        withTiming
            logger
            (text "LateOverloadedCallsCCs" <+> brackets (ppr this_mod))
            (\(binds, late_cc_state) -> seqBinds binds `seq` late_cc_state `seq` ())
            $ {-# SCC lateoverloadedCallsCCs #-} do
              pure $
                doLateCostCenters
                  lateCCConfig_env
                  (top_level_late_cc_state { lateCCState_extra = Strict.Nothing })
                  overloadedCallsCC
                  top_level_cc_binds
      else
        return
          ( top_level_cc_binds
          , top_level_late_cc_state { lateCCState_extra = Strict.Nothing }
          )

    when (lateCCConfig_whichBinds /= LateCCNone || lateCCConfig_overloadedCalls) $ do
      lintPassResult logger lateCCConfig_lintCfg top_level_cc_binds

    return (late_cc_binds, late_cc_state)
  where
    top_level_cc_pred :: CoreExpr -> Bool
    top_level_cc_pred =
        case lateCCConfig_whichBinds of
          LateCCBinds -> \rhs ->
            -- Make sure we record any functions. Even if it's something like `f = g`.
            mightBeFunTy (exprType rhs) ||
            -- If the RHS is a CAF doing work also insert a CC.
            not (exprIsWorkFree rhs)
          LateCCOverloadedBinds ->
            isOverloadedTy . exprType
          LateCCNone ->
            -- This is here for completeness, we won't actually use this
            -- predicate in this case since we'll shortcut.
            const False

    this_mod = lateCCEnv_module lateCCConfig_env
