module Rules.Generators.GhcPkgVersionHs (generateGhcPkgVersionHs) where

import Expression
import Oracles

generateGhcPkgVersionHs :: Expr String
generateGhcPkgVersionHs = do
    lift $ need [sourcePath -/- "Rules/Generators/GhcPkgVersionHs.hs"]
    projectVersion <- getSetting ProjectVersion
    targetOs       <- getSetting TargetOs
    targetArch     <- getSetting TargetArch
    return $ unlines
        [ "module Version where"
        , "version, targetOS, targetARCH :: String"
        , "version    = " ++ quote projectVersion
        , "targetOS   = " ++ quote targetOs
        , "targetARCH = " ++ quote targetArch ]
