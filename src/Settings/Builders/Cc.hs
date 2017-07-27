module Settings.Builders.Cc (ccBuilderArgs) where

import Settings.Builders.Common

ccBuilderArgs :: Args
ccBuilderArgs = do
  way <- getWay
  builder Cc ? mconcat
    [ append =<< getPkgDataList CcArgs
    , getSettingList . ConfCcArgs =<< getStage
    , cIncludeArgs

    , builder (Cc CompileC) ? mconcat
        [ arg "-Werror"
        , Dynamic `wayUnit` way ? append [ "-fPIC", "-DDYNAMIC" ]
        -- ref: mk/warning.mk:
        --  SRC_CC_OPTS     += -Wall $(WERROR)
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
