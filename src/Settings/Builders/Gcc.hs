module Settings.Builders.Gcc (gccArgs, gccMArgs) where

import Development.Shake.FilePath
import Expression
import Oracles
import Predicates (stagedBuilder)
import Settings
import Settings.Builders.Common (cIncludeArgs)

gccArgs :: Args
gccArgs = stagedBuilder Gcc ?
    mconcat [ commonGccArgs
            , arg "-c", arg =<< getInput
            , arg "-o", arg =<< getOutput ]

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
gccMArgs :: Args
gccMArgs = stagedBuilder GccM ? do
    output <- getOutput
    mconcat [ arg "-E"
            , arg "-MM"
            , commonGccArgs
            , arg "-MF"
            , arg output
            , arg "-MT"
            , arg $ dropExtension output -<.> "o"
            , arg "-x"
            , arg "c"
            , arg =<< getInput ]

commonGccArgs :: Args
commonGccArgs = mconcat [ append =<< getPkgDataList CcArgs
                        , cIncludeArgs ]
