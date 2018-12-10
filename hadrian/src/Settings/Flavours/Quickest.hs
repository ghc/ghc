module Settings.Flavours.Quickest (quickestFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Common

-- Please update doc/flavours.md when changing this file.
quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays     = pure [vanilla, threaded]
    , dynamicGhcPrograms = return False }

quickestArgs :: Args
quickestArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        , naturalInBaseFixArgs
        ]
    , hsLibrary  = mempty
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = stage0 ? arg "-O" }
