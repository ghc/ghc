module Settings.Builders.Configure (configureBuilderArgs) where

import Settings.Builders.Common

configureBuilderArgs :: Args
configureBuilderArgs = mconcat
    [ builder (Configure gmpBuildPath) ? do
        hostPlatform  <- getSetting HostPlatform
        buildPlatform <- getSetting BuildPlatform
        append [ "--enable-shared=no"
               , "--host=" ++ hostPlatform
               , "--build=" ++ buildPlatform ]

    , builder (Configure libffiBuildPath) ? do
        top            <- getTopDirectory
        targetPlatform <- getSetting TargetPlatform
        append [ "--prefix=" ++ top -/- libffiBuildPath -/- "inst"
               , "--libdir=" ++ top -/- libffiBuildPath -/- "inst/lib"
               , "--enable-static=yes"
               , "--enable-shared=no" -- TODO: add support for yes
               , "--host=" ++ targetPlatform ] ]
