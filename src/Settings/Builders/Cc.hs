module Settings.Builders.Cc (ccBuilderArgs) where

import Settings.Builders.Common

ccBuilderArgs :: Args
ccBuilderArgs = do
    way <- getWay
    builder Cc ? mconcat
        [ getPkgDataList CcArgs
        , getStagedSettingList ConfCcArgs
        , cIncludeArgs

        , builder (Cc CompileC) ? mconcat
            [ pure ["-Wall", "-Werror"]
            , Dynamic `wayUnit` way ? pure [ "-fPIC", "-DDYNAMIC" ]
            , arg "-c", arg =<< getInput
            , arg "-o", arg =<< getOutput ]

        , builder (Cc FindCDependencies) ? do
            output <- getOutput
            mconcat [ arg "-E"
                    , arg "-MM", arg "-MG"
                    , arg "-MF", arg output
                    , arg "-MT", arg $ dropExtension output -<.> "o"
                    , arg "-x", arg "c"
                    , arg =<< getInput ] ]
