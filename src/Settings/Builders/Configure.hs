module Settings.Builders.Configure (configureBuilderArgs) where

import Base
import Oracles.Config.Setting
import Predicate
import Settings

configureBuilderArgs :: Args
configureBuilderArgs = mconcat
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
