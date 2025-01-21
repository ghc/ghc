module GHC.Unit.Module.Stage where

import GHC.Prelude
import GHC.Stack
import GHC.Utils.Outputable

data ModuleStage = RunStage | CompileStage deriving (Eq, Ord, Enum, Bounded)

allStages :: [ModuleStage]
allStages = [minBound .. maxBound]

minStage :: ModuleStage
minStage = RunStage
maxStage :: ModuleStage
maxStage = CompileStage

instance Outputable ModuleStage where
  ppr CompileStage = text "compile"
  ppr RunStage = text "run"

zeroStage :: ModuleStage
zeroStage = RunStage

todoStage :: HasCallStack => ModuleStage
todoStage -- = pprTrace "todoStage" callStackDoc
          = zeroStage

--moduleStageToThLevel :: ModuleStage -> Int
--moduleStageToThLevel (ModuleStage m) = m

decModuleStage, incModuleStage :: ModuleStage -> ModuleStage
incModuleStage RunStage = RunStage
incModuleStage CompileStage = RunStage

decModuleStage RunStage = CompileStage
decModuleStage CompileStage = RunStage
