module GHC.Driver.Config.Stg.Pipeline
  ( initStgPipelineOpts
  ) where

import GHC.Prelude

import Control.Monad (guard)

import GHC.Stg.Pipeline
import GHC.Stg.Utils

import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Stg.Lift
import GHC.Driver.Config.Stg.Ppr
import GHC.Driver.DynFlags

-- | Initialize STG pretty-printing options from DynFlags
initStgPipelineOpts :: DynFlags -> Bool -> StgPipelineOpts
initStgPipelineOpts dflags for_bytecode =
  let !platform = targetPlatform dflags
      !ext_dyn_refs = gopt Opt_ExternalDynamicRefs dflags
  in StgPipelineOpts
    { stgPipeline_lint = do
        guard $ gopt Opt_DoStgLinting dflags
        Just $ initDiagOpts dflags
    , stgPipeline_pprOpts = initStgPprOpts dflags
    , stgPipeline_phases = getStgToDo for_bytecode dflags
    , stgPlatform = platform
    , stgPipeline_forBytecode = for_bytecode
    , stgPipeline_allowTopLevelConApp = allowTopLevelConApp platform ext_dyn_refs
    }

-- | Which Stg-to-Stg passes to run. Depends on flags, ways etc.
getStgToDo
  :: Bool -- ^ Are we preparing for bytecode?
  -> DynFlags
  -> [StgToDo]
getStgToDo for_bytecode dflags =
  filter (/= StgDoNothing)
    [ mandatory StgUnarise
    -- Important that unarisation comes first
    -- See Note [StgCse after unarisation] in GHC.Stg.CSE
    , optional Opt_StgCSE StgCSE
    , optional Opt_StgLiftLams $ StgLiftLams $ initStgLiftConfig dflags
    , runWhen for_bytecode StgBcPrep
    , optional Opt_StgStats StgStats
    ] where
      optional opt = runWhen (gopt opt dflags)
      mandatory = id

runWhen :: Bool -> StgToDo -> StgToDo
runWhen True todo = todo
runWhen _    _    = StgDoNothing
