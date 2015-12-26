module Rules.Generators.VersionHs (generateVersionHs) where

import Expression
import Oracles

generateVersionHs :: Expr String
generateVersionHs = do
    lift $ need [sourcePath -/- "Rules/Generators/VersionHs.hs"]
    projectVersion <- getSetting ProjectVersion
    targetOs       <- getSetting TargetOs
    targetArch     <- getSetting TargetArch
    return $ unlines
        [ "module Version where"
        , "version, targetOS, targetARCH :: String"
        , "version    = " ++ quote projectVersion
        , "targetOS   = " ++ quote targetOs
        , "targetARCH = " ++ quote targetArch ]
