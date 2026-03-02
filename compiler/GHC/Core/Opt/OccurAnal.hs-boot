module GHC.Core.Opt.OccurAnal where

import GHC.Prelude
import GHC.Core
import GHC.Types.Id (Id)
import GHC.Types.InlinePragma (ActivationGhc)
import GHC.Unit.Module (Module)

occurAnalyseCompUnit
  :: Module
  -> (Id -> Bool)
  -> (ActivationGhc -> Bool)
  -> [CoreRule]
  -> CoreCompUnit
  -> CoreCompUnit
