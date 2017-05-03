module Settings.Builders.Cc (ccBuilderArgs) where

import Settings.Builders.Common

ccBuilderArgs :: Args
ccBuilderArgs = builder Cc ? mconcat
    [ append =<< getPkgDataList CcArgs
    , argSettingList . ConfCcArgs =<< getStage
    , cIncludeArgs

    , builder (Cc CompileC) ? mconcat [ arg "-Werror"
                                      -- mk/warning.mk:
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
