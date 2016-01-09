module Rules.Generators.GhcPlatformH (generateGhcPlatformH) where

import Base
import Expression
import Oracles
import Rules.Generators.Common

generateGhcPlatformH :: Expr String
generateGhcPlatformH = do
    trackSource "Rules/Generators/GhcPlatformH.hs"
    hostPlatform   <- getSetting HostPlatform
    hostArch       <- getSetting HostArch
    hostOs         <- getSetting HostOs
    hostVendor     <- getSetting HostVendor
    targetPlatform <- getSetting TargetPlatform
    targetArch     <- getSetting TargetArch
    targetOs       <- getSetting TargetOs
    targetVendor   <- getSetting TargetVendor
    ghcUnreg       <- getFlag GhcUnregisterised
    return . unlines $
        [ "#ifndef __GHCPLATFORM_H__"
        , "#define __GHCPLATFORM_H__"
        , ""
        , "#define BuildPlatform_TYPE  " ++ cppify hostPlatform
        , "#define HostPlatform_TYPE   " ++ cppify targetPlatform
        , ""
        , "#define " ++ cppify hostPlatform   ++ "_BUILD 1"
        , "#define " ++ cppify targetPlatform ++ "_HOST 1"
        , ""
        , "#define " ++ hostArch   ++ "_BUILD_ARCH 1"
        , "#define " ++ targetArch ++ "_HOST_ARCH 1"
        , "#define BUILD_ARCH " ++ quote hostArch
        , "#define HOST_ARCH "  ++ quote targetArch
        , ""
        , "#define " ++ hostOs   ++ "_BUILD_OS 1"
        , "#define " ++ targetOs ++ "_HOST_OS 1"
        , "#define BUILD_OS " ++ quote hostOs
        , "#define HOST_OS "  ++ quote targetOs
        , ""
        , "#define " ++ hostVendor   ++ "_BUILD_VENDOR 1"
        , "#define " ++ targetVendor ++ "_HOST_VENDOR 1"
        , "#define BUILD_VENDOR " ++ quote hostVendor
        , "#define HOST_VENDOR "  ++ quote targetVendor
        , ""
        , "/* These TARGET macros are for backwards compatibility... DO NOT USE! */"
        , "#define TargetPlatform_TYPE " ++ cppify targetPlatform
        , "#define " ++ cppify targetPlatform ++ "_TARGET 1"
        , "#define " ++ targetArch ++ "_TARGET_ARCH 1"
        , "#define TARGET_ARCH " ++ quote targetArch
        , "#define " ++ targetOs ++ "_TARGET_OS 1"
        , "#define TARGET_OS " ++ quote targetOs
        , "#define " ++ targetVendor ++ "_TARGET_VENDOR 1" ]
        ++
        [ "#define UnregisterisedCompiler 1" | ghcUnreg ]
        ++
        [ "\n#endif /* __GHCPLATFORM_H__ */" ]
