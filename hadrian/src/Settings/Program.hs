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
    dynGhcProgs <- askDynGhcPrograms --dynamicGhcPrograms =<< flavour
    return $ Context stage pkg (wayFor profiled dynGhcProgs)

    where wayFor prof dyn
            | prof && dyn                          =
                error "programContext: profiling+dynamic not supported"
            | pkg == ghc && prof && stage > Stage0 = profiling
            | dyn && stage > Stage0                = dynamic
            | otherwise                            = vanilla
