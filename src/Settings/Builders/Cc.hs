module Settings.Builders.Cc (ccBuilderArgs) where

import Base
import Oracles.Config.Setting
import Oracles.PackageData
import Predicate
import Settings.Builders.Common
import Settings

ccBuilderArgs :: Args
ccBuilderArgs = builder Cc ? mconcat
    [ append =<< getPkgDataList CcArgs
    , argSettingList . ConfCcArgs =<< getStage
    , cIncludeArgs

    , builder (Cc CompileC) ?
        mconcat [ arg "-c", arg =<< getInput
                , arg "-o", arg =<< getOutput ]

    , builder (Cc FindCDependencies) ? do
        output <- getOutput
        mconcat [ arg "-E"
                , arg "-MM", arg "-MG"
                , arg "-MF", arg output
                , arg "-MT", arg $ dropExtension output -<.> "o"
                , arg "-x", arg "c"
                , arg =<< getInput ] ]
