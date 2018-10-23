module Settings.Builders.Cc (ccBuilderArgs) where

import Hadrian.Haskell.Cabal.Type
import Settings.Builders.Common

ccBuilderArgs :: Args
ccBuilderArgs = do
    way <- getWay
    builder Cc ? mconcat
        [ getContextData ccOpts
        , getStagedSettingList ConfCcArgs

        , builder (Cc CompileC) ? mconcat
            [ arg "-Wall"
            , cIncludeArgs
            , Dynamic `wayUnit` way ? pure [ "-fPIC", "-DDYNAMIC" ]
            , arg "-c", arg =<< getInput
            , arg "-o", arg =<< getOutput ]

        , builder (Cc FindCDependencies) ? do
            output <- getOutput
            mconcat [ arg "-E"
                    , arg "-MM", arg "-MG"
                    , arg "-MF", arg output
                    , arg "-MT", arg $ dropExtension output -<.> "o"
                    , cIncludeArgs
                    , arg "-x", arg "c"
                    , arg =<< getInput ] ]
