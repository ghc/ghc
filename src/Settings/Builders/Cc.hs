module Settings.Builders.Cc (ccBuilderArgs) where

import Development.Shake.FilePath
import Expression
import Oracles.Config.Setting
import Oracles.PackageData
import Predicates (builder)
import Settings
import Settings.Builders.Common (cIncludeArgs)

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
ccBuilderArgs :: Args
ccBuilderArgs = mconcat
    [ builder (Cc Compile) ?
        mconcat [ commonCcArgs
                , arg "-c", arg =<< getInput
                , arg "-o", arg =<< getOutput ]

    , builder (Cc FindDependencies) ? do
        output <- getOutput
        mconcat [ arg "-E"
                , arg "-MM"
                , commonCcArgs
                , arg "-MF"
                , arg output
                , arg "-MT"
                , arg $ dropExtension output -<.> "o"
                , arg "-x"
                , arg "c"
                , arg =<< getInput ] ]

commonCcArgs :: Args
commonCcArgs = mconcat [ append =<< getPkgDataList CcArgs
                       , append =<< getSettingList . ConfCcArgs =<< getStage
                       , cIncludeArgs ]
