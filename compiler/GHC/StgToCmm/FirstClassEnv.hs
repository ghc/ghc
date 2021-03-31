module GHC.StgToCmm.FirstClassEnv (
      buildFirstClassEnv
  ) where

import GHC.Prelude
import GHC.Types.Id
import GHC.Cmm.Graph
import GHC.Utils.Panic
import GHC.StgToCmm.Monad
import GHC.Utils.Outputable

buildFirstClassEnv :: Id -> [Id] -> FCode (CgIdInfo, FCode CmmAGraph)
buildFirstClassEnv = pprPanic "buildFCEnv" $ text "buildFCEnv"
