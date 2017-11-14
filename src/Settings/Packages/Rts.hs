module Settings.Packages.Rts (
    rtsContext, rtsBuildPath, rtsConfIn, rtsPackageArgs, rtsLibffiLibrary
    ) where

import Base
import Expression
import Oracles.Flag
import Oracles.Setting
import Settings

-- | RTS is considered a Stage1 package. This determines RTS build directory.
rtsContext :: Context
rtsContext = vanillaContext Stage1 rts

-- | Path to the RTS build directory.
rtsBuildPath :: Action FilePath
rtsBuildPath = buildPath rtsContext

-- | Path to RTS package configuration file, to be processed by HsCpp.
rtsConfIn :: FilePath
rtsConfIn = pkgPath rts -/- "package.conf.in"

-- These numbers can be found at:
-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa383745(v=vs.85).aspx
-- If we're compiling on windows, enforce that we only support Vista SP1+
-- Adding this here means it doesn't have to be done in individual .c files
-- and also centralizes the versioning.
-- | Minimum supported Windows version.
windowsVersion :: String
windowsVersion = "0x06000100"

libffiLibraryName :: Action FilePath
libffiLibraryName = do
    useSystemFfi <- flag UseSystemFfi
    windows      <- windowsHost
    return $ case (useSystemFfi, windows) of
        (True , False) -> "ffi"
        (False, False) -> "Cffi"
        (_    , True ) -> "Cffi-6"

rtsLibffiLibrary :: Way -> Action FilePath
rtsLibffiLibrary way = do
    name    <- libffiLibraryName
    suf     <- libsuf way
    rtsPath <- rtsBuildPath
    return $ rtsPath -/- "lib" ++ name ++ suf

-- Compile various performance-critical pieces *without* -fPIC -dynamic
-- even when building a shared library.  If we don't do this, then the
-- GC runs about 50% slower on x86 due to the overheads of PIC.  The
-- cost of doing this is a little runtime linking and less sharing, but
-- not much.
--
-- On x86_64 this doesn't work, because all objects in a shared library
-- must be compiled with -fPIC (since the 32-bit relocations generated
-- by the default small memory can't be resolved at runtime).  So we
-- only do this on i386.
--
-- This apparently doesn't work on OS X (Darwin) nor on Solaris.
-- On Darwin we get errors of the form
--
--  ld: absolute addressing (perhaps -mdynamic-no-pic) used in _stg_ap_0_fast from rts/dist/build/Apply.dyn_o not allowed in slidable image
--
-- and lots of these warnings:
--
--  ld: warning codegen in _stg_ap_pppv_fast (offset 0x0000005E) prevents image from loading in dyld shared cache
--
-- On Solaris we get errors like:
--
-- Text relocation remains                         referenced
--     against symbol                  offset      in file
-- .rodata (section)                   0x11        rts/dist/build/Apply.dyn_o
--   ...
-- ld: fatal: relocations remain against allocatable but non-writable sections
-- collect2: ld returned 1 exit status
speedHack :: Action Bool
speedHack = do
    i386 <- anyTargetArch ["i386"]
    goodOS <- not <$> anyTargetOs ["darwin", "solaris2"]
    return $ i386 && goodOS

rtsPackageArgs :: Args
rtsPackageArgs = package rts ? do
    projectVersion <- getSetting ProjectVersion
    hostPlatform   <- getSetting HostPlatform
    hostArch       <- getSetting HostArch
    hostOs         <- getSetting HostOs
    hostVendor     <- getSetting HostVendor
    buildPlatform  <- getSetting BuildPlatform
    buildArch      <- getSetting BuildArch
    buildOs        <- getSetting BuildOs
    buildVendor    <- getSetting BuildVendor
    targetPlatform <- getSetting TargetPlatform
    targetArch     <- getSetting TargetArch
    targetOs       <- getSetting TargetOs
    targetVendor   <- getSetting TargetVendor
    ghcUnreg       <- expr $ yesNo <$> flag GhcUnregisterised
    ghcEnableTNC   <- expr $ yesNo <$> ghcEnableTablesNextToCode
    way            <- getWay
    path           <- getBuildPath
    top            <- expr topDirectory
    libffiName     <- expr libffiLibraryName
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    ghclibDir      <- expr installGhcLibDir
    destDir        <- expr getDestDir
    let cArgs = mconcat
          [ arg "-Irts"
          , rtsWarnings
          , arg $ "-I" ++ path
          , flag UseSystemFfi ? arg ("-I" ++ ffiIncludeDir)
          , arg $ "-DRtsWay=\"rts_" ++ show way ++ "\""
          -- RTS *must* be compiled with optimisations. The INLINE_HEADER macro
          -- requires that functions are inlined to work as expected. Inlining
          -- only happens for optimised builds. Otherwise we can assume that
          -- there is a non-inlined variant to use instead. But RTS does not
          -- provide non-inlined alternatives and hence needs the function to
          -- be inlined. See https://github.com/snowleopard/hadrian/issues/90.
          , arg "-O2"

          , Debug     `wayUnit` way          ? arg "-DDEBUG"
          , way `elem` [debug, debugDynamic] ? arg "-DTICKY_TICKY"
          , Profiling `wayUnit` way          ? arg "-DPROFILING"
          , Threaded  `wayUnit` way          ? arg "-DTHREADED_RTS"

          , inputs ["//RtsMessages.c", "//Trace.c"] ?
            arg ("-DProjectVersion=" ++ show projectVersion)

          , input "//RtsUtils.c" ? pure
            [ "-DProjectVersion="            ++ show projectVersion
            , "-DHostPlatform="              ++ show hostPlatform
            , "-DHostArch="                  ++ show hostArch
            , "-DHostOS="                    ++ show hostOs
            , "-DHostVendor="                ++ show hostVendor
            , "-DBuildPlatform="             ++ show buildPlatform
            , "-DBuildArch="                 ++ show buildArch
            , "-DBuildOS="                   ++ show buildOs
            , "-DBuildVendor="               ++ show buildVendor
            , "-DTargetPlatform="            ++ show targetPlatform
            , "-DTargetArch="                ++ show targetArch
            , "-DTargetOS="                  ++ show targetOs
            , "-DTargetVendor="              ++ show targetVendor
            , "-DGhcUnregisterised="         ++ show ghcUnreg
            , "-DGhcEnableTablesNextToCode=" ++ show ghcEnableTNC ]

            , inputs ["//Evac.c", "//Evac_thr.c"] ? arg "-funroll-loops"

            , speedHack ?
              inputs [ "//Evac.c", "//Evac_thr.c"
                     , "//Scav.c", "//Scav_thr.c"
                     , "//Compact.c", "//GC.c" ] ? arg "-fno-PIC"
            -- -static is also necessary for these bits, otherwise the NCG
            -- generates dynamic references:
            , speedHack ?
              inputs [ "//Updates.c", "//StgMiscClosures.c"
                     , "//PrimOps.c", "//Apply.c"
                     , "//AutoApply.c" ] ? pure ["-fno-PIC", "-static"]

            -- inlining warnings happen in Compact
            , inputs ["//Compact.c"] ? arg "-Wno-inline"

            -- emits warnings about call-clobbered registers on x86_64
            , inputs [ "//RetainerProfile.c", "//StgCRun.c"
                     , "//win32/ConsoleHandler.c", "//win32/ThrIOManager.c"] ? arg "-w"
            , inputs ["//RetainerSet.c"] ? arg "-Wno-format"
            -- The above warning suppression flags are a temporary kludge.
            -- While working on this module you are encouraged to remove it and fix
            -- any warnings in the module. See:
            -- http://ghc.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings

            , (not <$> flag GccIsClang) ?
              inputs ["//Compact.c"] ? arg "-finline-limit=2500"

            , inputs ["//Evac_thr.c", "//Scav_thr.c"] ?
              pure ["-DPARALLEL_GC", "-Irts/sm"]

            , input "//StgCRun.c" ? windowsHost ? arg "-Wno-return-local-addr"
            , input "//RetainerProfile.c" ? flag GccIsClang ?
              arg "-Wno-incompatible-pointer-types"
            , windowsHost ? arg ("-DWINVER=" ++ windowsVersion)

            -- libffi's ffi.h triggers various warnings
            , inputs [ "//Interpreter.c", "//Storage.c", "//Adjustor.c" ] ?
              arg "-Wno-strict-prototypes"
            , inputs ["//Interpreter.c", "//Adjustor.c", "//sm/Storage.c"] ?
              anyTargetArch ["powerpc"] ? arg "-Wno-undef"
            ]

    mconcat
        [ builder (Cc FindCDependencies) ? cArgs
        , builder (Ghc CompileCWithGhc) ? map ("-optc" ++) <$> cArgs
        , builder Ghc ? arg "-Irts"

          , builder HsCpp ? pure
          [ "-DTOP="             ++ show top
          , "-DFFI_INCLUDE_DIR=" ++ show ffiIncludeDir
          , "-DFFI_LIB_DIR="     ++ show ffiLibraryDir
          , "-DFFI_LIB="         ++ show libffiName ]

        , builder HsCpp ?
          input "//package.conf.in" ?
          output "//package.conf.install.raw" ?
          pure [ "-DINSTALLING"
               , "-DLIB_DIR=\"" ++ destDir ++ ghclibDir ++ "\""
               , "-DINCLUDE_DIR=\"" ++ destDir ++ ghclibDir -/- "include\"" ]

        , builder HsCpp ? flag HaveLibMingwEx ? arg "-DHAVE_LIBMINGWEX" ]

-- See @rts/ghc.mk@.
rtsWarnings :: Args
rtsWarnings = mconcat
    [ pure ["-Wall", "-Werror"]
    , arg "-Wextra"
    , arg "-Wstrict-prototypes"
    , arg "-Wmissing-prototypes"
    , arg "-Wmissing-declarations"
    , arg "-Winline"
    , arg "-Waggregate-return"
    , arg "-Wpointer-arith"
    , arg "-Wmissing-noreturn"
    , arg "-Wnested-externs"
    , arg "-Wredundant-decls"
    , arg "-Wundef"
    , arg "-fno-strict-aliasing" ]
