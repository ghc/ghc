module Settings.Flavours.Quickest (quickestFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays     = quickestRtsWays }

quickestArgs :: Args
quickestArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = mempty
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = stage0 ? arg "-O" }

-- Replicate GHCs RtsWays for flavour quickest (without dynamic):
-- $ make show! VALUE=GhcLibWays
-- GhcLibWays="v"
-- $ make show! VALUE=GhcRTSWays
-- GhcRTSWays="l debug thr thr_debug thr_l"
quickestRtsWays :: Ways
quickestRtsWays = pure [vanilla, logging, debug, threaded, threadedDebug, threadedLogging]
