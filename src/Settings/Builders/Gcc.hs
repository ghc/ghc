module Settings.Builders.Gcc (gccArgs, gccMArgs) where

import Development.Shake.FilePath
import Expression
import GHC
import Oracles
import Base ((-/-))
import Predicates (package, stagedBuilder)
import Settings

-- TODO: I had to define symbol __GLASGOW_HASKELL__ as otherwise directory.c is
-- effectively empty. I presume it was expected that GHC will be used for
-- compiling all C files, but I don't know why. It seems that directory.c is the
-- only file which requires special treatment when using GCC.
gccArgs :: Args
gccArgs = stagedBuilder Gcc ?
    mconcat [ commonGccArgs
            , package directory ? arg "-D__GLASGOW_HASKELL__"
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
commonGccArgs = do
    pkg    <- getPackage
    path   <- getTargetPath
    iDirs  <- getPkgDataList IncludeDirs
    dDirs  <- getPkgDataList DepIncludeDirs
    ccArgs <- getPkgDataList CcArgs
    mconcat [ append ccArgs
            , arg $ "-I" ++ path -/- "build/autogen"
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- iDirs ++ dDirs ]]
