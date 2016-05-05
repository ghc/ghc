module Settings.Builders.Configure (configureArgs) where

import Base
import Expression
import Oracles.Config.Setting
import Predicates (builder)
import Settings

configureArgs :: Args
configureArgs = mconcat
    [ builder (Configure libffiBuildPath) ? do
        top            <- getTopDirectory
        targetPlatform <- getSetting TargetPlatform
        mconcat [ arg $ "--prefix=" ++ top -/- libffiBuildPath -/- "inst"
                , arg $ "--libdir=" ++ top -/- libffiBuildPath -/- "inst/lib"
                , arg $ "--enable-static=yes"
                , arg $ "--enable-shared=no" -- TODO: add support for yes
                , arg $ "--host=" ++ targetPlatform ]

    , builder (Configure gmpBuildPath) ? do
        hostPlatform  <- getSetting HostPlatform
        buildPlatform <- getSetting BuildPlatform
        mconcat [ arg $ "--enable-shared=no"
                , arg $ "--host=" ++ hostPlatform
                , arg $ "--build=" ++ buildPlatform ] ]
