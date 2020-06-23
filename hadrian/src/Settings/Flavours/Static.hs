module Settings.Flavours.Static (staticFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
staticFlavour :: Flavour
staticFlavour = defaultFlavour
    { name = "static"
    , args = defaultBuilderArgs <> staticArgs <> defaultPackageArgs }

staticFlags :: Args
staticFlags = mconcat
    [ stage0 ? arg "-O"
    , notStage0 ? arg "-O2"
    , pure [ "-static" , "-optl", "-static" ]
    ]

staticArgs :: Args
staticArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O", "-H64m"]
    , hsLibrary  = staticFlags
    , hsCompiler = pure ["-O2"]
    , hsGhc      = staticFlags }
