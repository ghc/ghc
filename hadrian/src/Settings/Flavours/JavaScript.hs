module Settings.Flavours.JavaScript
 ( quickJsFlavour
 , perfJsFlavour
 , releaseJsFlavour
 ) where

import qualified Data.Set as Set

import Flavour
import Expression
import Settings.Flavours.Performance
import {-# SOURCE #-} Settings.Default

releaseJsFlavour :: Flavour
releaseJsFlavour =   disableDynamicLibs
                   . disableDynamicGhcPrograms
                   . disableProfiledLibs
                   . enableO2Stage0
                   . useNativeBignum
                   $ performanceFlavour { name = "release-js" }

quickJsFlavour :: Flavour
quickJsFlavour = defaultFlavour
    { name        = "quick-js"
    , args        = defaultBuilderArgs <> quickJsArgs <> defaultPackageArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = pure $ Set.singleton vanilla
    , rtsWays     = pure $ Set.singleton vanilla
    }

perfJsFlavour :: Flavour
perfJsFlavour = defaultFlavour
    { name        = "perf-js"
    , args        = defaultBuilderArgs <> perfJsArgs <> defaultPackageArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = pure $ Set.singleton vanilla
    , rtsWays     = pure $ Set.singleton vanilla
    }

quickJsArgs :: Args
quickJsArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O" ]
    , hsCompiler = stage0 ? arg "-O2"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0" ] ] }

perfJsArgs :: Args
perfJsArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ arg "-O2", arg "-H64m"]
    , hsLibrary  = arg "-O2"
    , hsCompiler = arg "-O2"
    , hsGhc      = arg "-O2"
    }
