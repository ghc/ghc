module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Settings.Builders.Common

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage   <- getStage
    root    <- getBuildRoot
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    mconcat [ getSettingList HsCppArgs
            , arg "-P"
            , arg "-Iincludes"
            , arg $ "-I" ++ root -/- generatedDir
            , arg $ "-I" ++ ghcPath
            , arg "-x", arg "c"
            , arg =<< getInput ]
