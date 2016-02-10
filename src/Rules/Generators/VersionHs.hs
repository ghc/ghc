module Rules.Generators.VersionHs (generateVersionHs) where

import Base
import Expression
import Oracles.Config.Setting
import Rules.Generators.Common

generateVersionHs :: Expr String
generateVersionHs = do
    trackSource "Rules/Generators/VersionHs.hs"
    projectVersion <- getSetting ProjectVersion
    targetOs       <- getSetting TargetOs
    targetArch     <- getSetting TargetArch
    return $ unlines
        [ "module Version where"
        , "version, targetOS, targetARCH :: String"
        , "version    = " ++ quote projectVersion
        , "targetOS   = " ++ quote targetOs
        , "targetARCH = " ++ quote targetArch ]
