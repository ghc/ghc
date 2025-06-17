module Settings.Program
  ( programContext
  , ghcWithInterpreter
  ) where

import Base
import Context
import Oracles.Flavour
import Oracles.Flag
import Packages

import GHC.Platform.ArchOS
import Settings.Builders.Common (anyTargetOs, anyTargetArch, isArmTarget)

-- TODO: there is duplication and inconsistency between this and
-- Rules.Program.getProgramContexts. There should only be one way to
-- get a context/contexts for a given stage and package.
programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- askGhcProfiled stage
    dynGhcProgs <- askDynGhcPrograms --dynamicGhcPrograms =<< flavour
    return $ Context stage pkg (wayFor profiled dynGhcProgs) Final

    where wayFor prof dyn
            | prof && dyn                          = profilingDynamic
            | pkg == ghc && prof && notStage0 stage = profiling
            | dyn && notStage0 stage                = dynamic
            | otherwise                            = vanilla

          notStage0 (Stage0 {}) = False
          notStage0 _ = True

-- | When cross compiling, enable for stage0 to get ghci
-- support. But when not cross compiling, disable for
-- stage0, otherwise we introduce extra dependencies
-- like haskeline etc, and mixing stageBoot/stage0 libs
-- can cause extra trouble (e.g. #25406)
--
-- Also checks whether the target supports GHCi.
ghcWithInterpreter :: Stage -> Action Bool
ghcWithInterpreter stage = do
    is_cross <- flag CrossCompiling
    goodOs <- anyTargetOs [ OSMinGW32, OSLinux, OSSolaris2 -- TODO "cygwin32"?,
                          , OSFreeBSD, OSDragonFly, OSNetBSD, OSOpenBSD
                          , OSDarwin, OSKFreeBSD
                          , OSWasi ]
    goodArch <- (||) <$>
                anyTargetArch [ ArchX86, ArchX86_64, ArchPPC
                              , ArchAArch64, ArchS390X
                              , ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2
                              , ArchRISCV64, ArchLoongArch64
                              , ArchWasm32 ]
                              <*> isArmTarget
    -- The explicit support list is essentially a list of platforms for which
    -- the RTS linker has support. If the RTS linker is not supported then we
    -- fall back on dynamic linking:
    dynamicGhcProgs <- askDynGhcPrograms

    -- Maybe this should just be false for cross compilers. But for now
    -- I've kept the old behaviour where it will say yes. (See #25939)
    return $ ((goodOs && goodArch) || dynamicGhcProgs) && (stage >= Stage1 || is_cross)
