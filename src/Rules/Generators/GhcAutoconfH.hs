module Rules.Generators.GhcAutoconfH (generateGhcAutoconfH, configH) where

import Base
import Expression
import Oracles.Config.Flag
import Oracles.Config.Setting
import Rules.Generators.Common

-- TODO: change `mk/config.h` to `shake-build/cfg/config.h`
configH :: FilePath
configH = "mk/config.h"

undefinePackage :: String -> String
undefinePackage s
    | "#define PACKAGE_" `isPrefixOf` s
                = "/* #undef " ++ takeWhile (/=' ') (drop 8 s) ++ " */"
    | otherwise = s

generateGhcAutoconfH :: Expr String
generateGhcAutoconfH = do
    trackSource "Rules/Generators/GhcAutoconfH.hs"
    configHContents  <- expr $ map undefinePackage <$> readFileLines configH
    tablesNextToCode <- expr ghcEnableTablesNextToCode
    ghcUnreg         <- getFlag GhcUnregisterised
    ccLlvmBackend    <- getSetting CcLlvmBackend
    ccClangBackend   <- getSetting CcClangBackend
    return . unlines $
        [ "#ifndef __GHCAUTOCONF_H__"
        , "#define __GHCAUTOCONF_H__" ]
        ++ configHContents ++
        [ "\n#define TABLES_NEXT_TO_CODE 1" | tablesNextToCode && not ghcUnreg ]
        ++
        [ "\n#define llvm_CC_FLAVOR 1"      | ccLlvmBackend == "1" ]
        ++
        [ "\n#define clang_CC_FLAVOR 1"     | ccClangBackend == "1" ]
        ++
        [ "#endif /* __GHCAUTOCONF_H__ */" ]
