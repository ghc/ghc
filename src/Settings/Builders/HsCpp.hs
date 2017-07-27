module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Settings.Builders.Common

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage <- getStage
    mconcat [ getSettingList HsCppArgs
            , arg "-P"
            , arg "-Iincludes"
            , arg $ "-I" ++ generatedPath
            , arg $ "-I" ++ buildPath (vanillaContext stage compiler)
            , arg "-x", arg "c"
            , arg =<< getInput ]
