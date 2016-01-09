module Rules.Generators.GhcBootPlatformH (generateGhcBootPlatformH) where

import Base
import Expression
import Oracles
import Rules.Generators.Common

generateGhcBootPlatformH :: Expr String
generateGhcBootPlatformH = do
    trackSource "Rules/Generators/GhcBootPlatformH.hs"
    stage <- getStage
    let chooseSetting x y = getSetting $ if stage == Stage0 then x else y
    buildPlatform  <- chooseSetting BuildPlatform HostPlatform
    buildArch      <- chooseSetting BuildArch     HostArch
    buildOs        <- chooseSetting BuildOs       HostOs
    buildVendor    <- chooseSetting BuildVendor   HostVendor
    hostPlatform   <- chooseSetting HostPlatform  TargetPlatform
    hostArch       <- chooseSetting HostArch      TargetArch
    hostOs         <- chooseSetting HostOs        TargetOs
    hostVendor     <- chooseSetting HostVendor    TargetVendor
    targetPlatform <- getSetting TargetPlatform
    targetArch     <- getSetting TargetArch
    targetOs       <- getSetting TargetOs
    targetVendor   <- getSetting TargetVendor
    return $ unlines
        [ "#ifndef __PLATFORM_H__"
        , "#define __PLATFORM_H__"
        , ""
        , "#define BuildPlatform_NAME  " ++ quote buildPlatform
        , "#define HostPlatform_NAME   " ++ quote hostPlatform
        , "#define TargetPlatform_NAME " ++ quote targetPlatform
        , ""
        , "#define " ++ cppify buildPlatform  ++ "_BUILD 1"
        , "#define " ++ cppify hostPlatform   ++ "_HOST 1"
        , "#define " ++ cppify targetPlatform ++ "_TARGET 1"
        , ""
        , "#define " ++ buildArch  ++ "_BUILD_ARCH 1"
        , "#define " ++ hostArch   ++ "_HOST_ARCH 1"
        , "#define " ++ targetArch ++ "_TARGET_ARCH 1"
        , "#define BUILD_ARCH "  ++ quote buildArch
        , "#define HOST_ARCH "   ++ quote hostArch
        , "#define TARGET_ARCH " ++ quote targetArch
        , ""
        , "#define " ++ buildOs  ++ "_BUILD_OS 1"
        , "#define " ++ hostOs   ++ "_HOST_OS 1"
        , "#define " ++ targetOs ++ "_TARGET_OS 1"
        , "#define BUILD_OS "  ++ quote buildOs
        , "#define HOST_OS "   ++ quote hostOs
        , "#define TARGET_OS " ++ quote targetOs
        , ""
        , "#define " ++ buildVendor  ++ "_BUILD_VENDOR 1"
        , "#define " ++ hostVendor   ++ "_HOST_VENDOR 1"
        , "#define " ++ targetVendor ++ "_TARGET_VENDOR  1"
        , "#define BUILD_VENDOR "  ++ quote buildVendor
        , "#define HOST_VENDOR "   ++ quote hostVendor
        , "#define TARGET_VENDOR " ++ quote targetVendor
        , ""
        , "#endif /* __PLATFORM_H__ */" ]
