module Settings.Flavours.Static (staticFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
staticFlavour :: Flavour
staticFlavour = defaultFlavour
    { name = "static"
    , args = defaultBuilderArgs <> perfArgs <> defaultPackageArgs <> staticExec
    , dynamicGhcPrograms = return False }

staticExec :: Args
staticExec = mconcat
    [ builder (Ghc LinkHs) ? pure [ "-optl", "-static" ]
    , builder (Ghc CompileCWithGhc) ? pure [ "-optc", "-static" ]
    , pure [ "-static" , "-fPIC" ] ]

perfArgs :: Args
perfArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O2"
    , hsCompiler = pure ["-O2"]
    , hsGhc      = mconcat [stage0 ? arg "-O", notStage0 ? arg "-O2"] }
