module Settings.Builders.Common (includesArgs, cIncludeArgs) where

import Base
import Expression
import Oracles.PackageData
import Settings

includes :: [FilePath]
includes = [ "includes", "includes/dist-derivedconstants/header" ]

includesArgs :: Args
includesArgs = append $ map ("-I" ++) includes

cIncludeArgs :: Args
cIncludeArgs = do
    stage   <- getStage
    pkg     <- getPackage
    incDirs <- getPkgDataList IncludeDirs
    depDirs <- getPkgDataList DepIncludeDirs
    let buildPath = targetPath stage pkg -/- "build"
    mconcat [ arg $ "-I" ++ buildPath
            , arg $ "-I" ++ buildPath -/- "autogen"
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- incDirs ]
            , append [ "-I" ++                 dir | dir <- depDirs ] ]
