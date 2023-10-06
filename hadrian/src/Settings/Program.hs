module Settings.Program
  ( programContext
  ) where

import Base
import Context
import Oracles.Flavour
import Packages

-- TODO: there is duplication and inconsistency between this and
-- Rules.Program.getProgramContexts. There should only be one way to
-- get a context/contexts for a given stage and package.
programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- askGhcProfiled stage
    dynGhcProgs <- askDynGhcPrograms stage
    -- Have to build static if it's a cross stage as we won't distribute the libraries built for the host.
    return $ Context stage pkg (wayFor profiled dynGhcProgs) Final

    where wayFor prof dyn
            | prof && dyn                          = profilingDynamic
            | pkg == ghc && prof && notStage0 stage = profiling
            | dyn && notStage0 stage                = dynamic
            | otherwise                             = vanilla

          notStage0 (Stage0 {}) = False
          notStage0 _ = True
