module Settings.Builders.Configure (configureBuilderArgs) where

import Settings.Builders.Common

configureBuilderArgs :: Args
configureBuilderArgs = mconcat
    [ builder (Configure gmpBuildPath) ? do
        hostPlatform  <- getSetting HostPlatform
        buildPlatform <- getSetting BuildPlatform
        pure [ "--enable-shared=no"
             , "--host=" ++ hostPlatform
             , "--build=" ++ buildPlatform ]

    , builder (Configure libffiBuildPath) ? do
        top            <- expr topDirectory
        targetPlatform <- getSetting TargetPlatform
        pure [ "--prefix=" ++ top -/- libffiBuildPath -/- "inst"
             , "--libdir=" ++ top -/- libffiBuildPath -/- "inst/lib"
             , "--enable-static=yes"
             , "--enable-shared=no" -- TODO: add support for yes
             , "--host=" ++ targetPlatform ] ]
