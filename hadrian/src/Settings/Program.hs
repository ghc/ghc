module Settings.Program
  ( programContext
  ) where

import Base
import Context
import Oracles.Flavour
import Oracles.Setting
import Packages

-- TODO: there is duplication and inconsistency between this and
-- Rules.Program.getProgramContexts. There should only be one way to
-- get a context/contexts for a given stage and package.
programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- askGhcProfiled stage
    dynGhcProgs <- askDynGhcPrograms stage --dynamicGhcPrograms =<< flavour
    -- Have to build static if it's a cross stage as we won't distribute the libraries built for the host.
    cross <- crossStage stage
    return $ Context stage pkg (wayFor profiled dynGhcProgs cross) Final

    where wayFor prof dyn cross
            | prof && dyn                           =
                error "programContext: profiling+dynamic not supported"
            | pkg == ghc && prof && notStage0 stage = profiling
            | dyn && notStage0 stage && not cross   = dynamic
            | otherwise                             = vanilla

          notStage0 (Stage0 {}) = False
          notStage0 _ = True
