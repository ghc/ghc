module Settings.Flavours.Static (staticFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

import Settings.Flavours.Performance (performanceArgs)

-- Please update doc/flavours.md when changing this file.
staticFlavour :: Flavour
staticFlavour = defaultFlavour
    { name = "static"
    , args = defaultBuilderArgs <> performanceArgs <> defaultPackageArgs <> staticExec
    , dynamicGhcPrograms = return False }


staticExec :: Args
staticExec = not . wayUnit Dynamic <$> getWay ? mconcat
    [ builder (Ghc CompileHs) ? pure [ "-fPIC", "-static" ]
    , builder (Ghc CompileCWithGhc) ? pure [ "-fPIC", "-optc", "-static"]
    , builder (Ghc LinkHs) ? pure [ "-optl", "-static" ]
    ]
