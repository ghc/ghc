
module GHC.HsToCore.Pmc.Ppr (pprUncovered) where

import GHC.Types.Id
import GHC.Utils.Outputable
import {-# SOURCE #-} GHC.HsToCore.Pmc.Solver.Types

pprUncovered :: Nabla -> [Id] -> SDoc
