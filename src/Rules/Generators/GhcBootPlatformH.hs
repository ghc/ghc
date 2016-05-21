module Rules.Generators.GhcBootPlatformH (generateGhcBootPlatformH) where

import Expression
import Oracles.Config.Setting
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
        , "#define BuildPlatform_NAME  " ++ show buildPlatform
        , "#define HostPlatform_NAME   " ++ show hostPlatform
        , "#define TargetPlatform_NAME " ++ show targetPlatform
        , ""
        , "#define " ++ cppify buildPlatform  ++ "_BUILD 1"
        , "#define " ++ cppify hostPlatform   ++ "_HOST 1"
        , "#define " ++ cppify targetPlatform ++ "_TARGET 1"
        , ""
        , "#define " ++ buildArch  ++ "_BUILD_ARCH 1"
        , "#define " ++ hostArch   ++ "_HOST_ARCH 1"
        , "#define " ++ targetArch ++ "_TARGET_ARCH 1"
        , "#define BUILD_ARCH "  ++ show buildArch
        , "#define HOST_ARCH "   ++ show hostArch
        , "#define TARGET_ARCH " ++ show targetArch
        , ""
        , "#define " ++ buildOs  ++ "_BUILD_OS 1"
        , "#define " ++ hostOs   ++ "_HOST_OS 1"
        , "#define " ++ targetOs ++ "_TARGET_OS 1"
        , "#define BUILD_OS "  ++ show buildOs
        , "#define HOST_OS "   ++ show hostOs
        , "#define TARGET_OS " ++ show targetOs
        , ""
        , "#define " ++ buildVendor  ++ "_BUILD_VENDOR 1"
        , "#define " ++ hostVendor   ++ "_HOST_VENDOR 1"
        , "#define " ++ targetVendor ++ "_TARGET_VENDOR  1"
        , "#define BUILD_VENDOR "  ++ show buildVendor
        , "#define HOST_VENDOR "   ++ show hostVendor
        , "#define TARGET_VENDOR " ++ show targetVendor
        , ""
        , "#endif /* __PLATFORM_H__ */" ]
