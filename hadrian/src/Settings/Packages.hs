module Settings.Packages (packageArgs) where

import Expression
import Flavour
import Oracles.Setting
import Oracles.Flag
import Packages
import Rules.Gmp
import Settings

-- | Package-specific command-line arguments.
packageArgs :: Args
packageArgs = do
    stage        <- getStage
    rtsWays      <- getRtsWays
    path         <- getBuildPath
    intLib       <- getIntegerPackage
    compilerPath <- expr $ buildPath (vanillaContext stage compiler)
    gmpBuildPath <- expr gmpBuildPath
    let includeGmp = "-I" ++ gmpBuildPath -/- "include"

    mconcat
        --------------------------------- base ---------------------------------
        [ package base ? mconcat
          [ builder (Cabal Flags) ? notStage0 ? arg (pkgName intLib)

          -- This fixes the 'unknown symbol stat' issue.
          -- See: https://github.com/snowleopard/hadrian/issues/259.
          , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

        ------------------------------ bytestring ------------------------------
        , package bytestring ?
          builder (Cabal Flags) ? intLib == integerSimple ? arg "integer-simple"

        --------------------------------- cabal --------------------------------
        -- Cabal is a large library and slow to compile. Moreover, we build it
        -- for Stage0 only so we can link ghc-pkg against it, so there is little
        -- reason to spend the effort to optimise it.
        , package cabal ?
          stage0 ? builder Ghc ? arg "-O0"

        ------------------------------- compiler -------------------------------
        , package compiler ? mconcat
          [ builder Alex ? arg "--latin1"

          , builder (Ghc CompileHs) ? mconcat
            [ inputs ["//GHC.hs", "//GhcMake.hs"] ? arg "-fprof-auto"
            , input "//Parser.hs" ?
              pure ["-fno-ignore-interface-pragmas", "-fcmm-sink" ] ]

          , builder (Cabal Setup) ? mconcat
            [ arg $ "--ghc-option=-DSTAGE=" ++ show (fromEnum stage + 1)
            , arg "--disable-library-for-ghci"
            , anyTargetOs ["openbsd"] ? arg "--ld-options=-E"
            , flag GhcUnregisterised ? arg "--ghc-option=-DNO_REGS"
            , notM ghcWithSMP ? arg "--ghc-option=-DNOSMP"
            , notM ghcWithSMP ? arg "--ghc-option=-optc-DNOSMP"
            , (any (wayUnit Threaded) rtsWays) ?
              notStage0 ? arg "--ghc-option=-optc-DTHREADED_RTS"
            , ghcWithInterpreter ?
              ghcEnableTablesNextToCode ?
              notM (flag GhcUnregisterised) ?
              notStage0 ? arg "--ghc-option=-DGHCI_TABLES_NEXT_TO_CODE"
            , ghcWithInterpreter ?
              ghciWithDebugger <$> flavour ?
              notStage0 ? arg "--ghc-option=-DDEBUGGER"
            , ghcProfiled <$> flavour ?
              notStage0 ? arg "--ghc-pkg-option=--force" ]

         , builder (Cabal Flags) ? mconcat
            [ ghcWithNativeCodeGen ? arg "ncg"
            , ghcWithInterpreter ? notStage0 ? arg "ghci"
            , flag CrossCompiling ? arg "-terminfo"
            , notStage0 ? intLib == integerGmp ?
              arg "integer-gmp" ]

          , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]

        ---------------------------------- ghc ---------------------------------
        , package ghc ? mconcat
          [ builder Ghc ? arg ("-I" ++ compilerPath)

          , builder (Cabal Flags) ? mconcat
            [ ghcWithInterpreter ? notStage0 ? arg "ghci"
            , flag CrossCompiling ? arg "-terminfo"
            -- the 'threaded' flag is True by default, but
            -- let's record explicitly that we link all ghc
            -- executables with the threaded runtime.
            , arg "threaded" ] ]

        -------------------------------- ghcPkg --------------------------------
        , package ghcPkg ?
          builder (Cabal Flags) ? flag CrossCompiling ? arg "-terminfo"

        -------------------------------- ghcPrim -------------------------------
        , package ghcPrim ? mconcat
          [ builder (Cabal Flags) ? arg "include-ghc-prim"

          , builder (Cc CompileC) ? (not <$> flag GccIsClang) ?
            input "//cbits/atomic.c"  ? arg "-Wno-sync-nand" ]

        --------------------------------- ghci ---------------------------------
        -- TODO: This should not be @not <$> flag CrossCompiling@. Instead we
        -- should ensure that the bootstrap compiler has the same version as the
        -- one we are building.

        -- TODO: In that case we also do not need to build most of the Stage1
        -- libraries, as we already know that the compiler comes with the most
        -- recent versions.

        -- TODO: The use case here is that we want to build @ghc-proxy@ for the
        -- cross compiler. That one needs to be compiled by the bootstrap
        -- compiler as it needs to run on the host. Hence @libiserv@ needs
        -- @GHCi.TH@, @GHCi.Message@ and @GHCi.Run@ from @ghci@. And those are
        -- behind the @-fghci@ flag.
        , package ghci ? mconcat
          [ notStage0 ? builder (Cabal Flags) ? arg "ghci"
          , flag CrossCompiling ? stage0 ? builder (Cabal Flags) ? arg "ghci" ]

        -------------------------------- haddock -------------------------------
        , package haddock ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ------------------------------- haskeline ------------------------------
        , package haskeline ?
          builder (Cabal Flags) ? flag CrossCompiling ? arg "-terminfo"

        -------------------------------- hsc2hs --------------------------------
        , package hsc2hs ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ------------------------------ integerGmp ------------------------------
        , package integerGmp ? mconcat
          [ builder Cc ? arg includeGmp

          , builder (Cabal Setup) ? mconcat
            [ -- TODO: This should respect some settings flag "InTreeGmp".
              -- Depending on @IncludeDir@ and @LibDir@ is bound to fail, since
              -- these are only set if the configure script was explicilty
              -- called with GMP include and lib dirs. Their absense as such
              -- does not imply @in-tree-gmp@.
              -- (null gmpIncludeDir && null gmpLibDir) ?
              -- arg "--configure-option=--with-intree-gmp"
              arg ("--configure-option=CFLAGS=" ++ includeGmp)
            , arg ("--gcc-options="             ++ includeGmp) ] ]

        ---------------------------------- rts ---------------------------------
        , package rts ? rtsPackageArgs -- RTS deserves a separate function

        -------------------------------- runGhc --------------------------------
        , package runGhc ?
          builder Ghc ? input "//Main.hs" ?
          (\version -> ["-cpp", "-DVERSION=" ++ show version]) <$> getSetting ProjectVersion

        --------------------------------- text ---------------------------------
        -- The package @text@ is rather tricky. It's a boot library, and it
        -- tries to determine on its own if it should link against @integer-gmp@
        -- or @integer-simple@. For Stage0, we need to use the integer library
        -- that the bootstrap compiler has (since @interger@ is not a boot
        -- library) and therefore we copy it over into the Stage0 package-db.
        -- Maybe we should stop doing this? And subsequently @text@ for Stage1
        -- detects the same integer library again, even though we don't build it
        -- in Stage1, and at that point the configuration is just wrong.
        , package text ?
          builder (Cabal Flags) ? notStage0 ? intLib == integerSimple ?
          pure [ "+integer-simple", "-bytestring-builder"] ]

-- | RTS-specific command line arguments.
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
    rtsWays        <- getRtsWays
    way            <- getWay
    path           <- getBuildPath
    top            <- expr topDirectory
    libffiName     <- expr libffiLibraryName
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    let cArgs = mconcat
          [ arg "-Irts"
          , rtsWarnings
          , arg $ "-I" ++ path
          , flag UseSystemFfi ? arg ("-I" ++ ffiIncludeDir)
          , arg $ "-DRtsWay=\"rts_" ++ show way ++ "\""
          -- Set the namespace for the rts fs functions
          , arg $ "-DFS_NAMESPACE=rts"
          , arg $ "-DCOMPILING_RTS"
          -- RTS *must* be compiled with optimisations. The INLINE_HEADER macro
          -- requires that functions are inlined to work as expected. Inlining
          -- only happens for optimised builds. Otherwise we can assume that
          -- there is a non-inlined variant to use instead. But RTS does not
          -- provide non-inlined alternatives and hence needs the function to
          -- be inlined. See https://github.com/snowleopard/hadrian/issues/90.
          , arg "-O2"
          , arg "-fomit-frame-pointer"
          , arg "-g"

          , Debug     `wayUnit` way          ? pure [ "-DDEBUG"
                                                    , "-fno-omit-frame-pointer"
                                                    , "-g" ]
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

          -- We're after pur performance here. So make sure fast math and
          -- vectorization is enabled.
          , input "//xxhash.c" ? pure
            [ "-O3"
            , "-ffast-math"
            , "-ftree-vectorize" ]

            , inputs ["//Evac.c", "//Evac_thr.c"] ? arg "-funroll-loops"

            , speedHack ?
              inputs [ "//Evac.c", "//Evac_thr.c"
                     , "//Scav.c", "//Scav_thr.c"
                     , "//Compact.c", "//GC.c" ] ? arg "-fno-PIC"
            -- @-static@ is necessary for these bits, as otherwise the NCG
            -- generates dynamic references.
            , speedHack ?
              inputs [ "//Updates.c", "//StgMiscClosures.c"
                     , "//PrimOps.c", "//Apply.c"
                     , "//AutoApply.c" ] ? pure ["-fno-PIC", "-static"]

            -- inlining warnings happen in Compact
            , inputs ["//Compact.c"] ? arg "-Wno-inline"

            -- emits warnings about call-clobbered registers on x86_64
            , inputs [ "//RetainerProfile.c", "//StgCRun.c"
                     , "//win32/ConsoleHandler.c", "//win32/ThrIOManager.c"] ? arg "-w"
            -- The above warning suppression flags are a temporary kludge.
            -- While working on this module you are encouraged to remove it and fix
            -- any warnings in the module. See:
            -- http://ghc.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings

            , (not <$> flag GccIsClang) ?
              inputs ["//Compact.c"] ? arg "-finline-limit=2500"

            , input "//RetainerProfile.c" ? flag GccIsClang ?
              arg "-Wno-incompatible-pointer-types"
            , windowsHost ? arg ("-DWINVER=" ++ windowsVersion)

            -- libffi's ffi.h triggers various warnings
            , inputs [ "//Interpreter.c", "//Storage.c", "//Adjustor.c" ] ?
              arg "-Wno-strict-prototypes"
            , inputs ["//Interpreter.c", "//Adjustor.c", "//sm/Storage.c"] ?
              anyTargetArch ["powerpc"] ? arg "-Wno-undef" ]

    mconcat
        [ builder (Cabal Flags) ? mconcat
          [ any (wayUnit Profiling) rtsWays ? arg "profiling"
          , any (wayUnit Debug) rtsWays ? arg "debug"
          , any (wayUnit Logging) rtsWays ? arg "logging"
          , any (wayUnit Dynamic) rtsWays ? arg "dynamic"
          , Debug `wayUnit` way           ? arg "find-ptr"
          ]
        , builder (Cc FindCDependencies) ? cArgs
        , builder (Ghc CompileCWithGhc) ? map ("-optc" ++) <$> cArgs
        , builder Ghc ? arg "-Irts"

          , builder HsCpp ? pure
          [ "-DTOP="             ++ show top
          , "-DFFI_INCLUDE_DIR=" ++ show ffiIncludeDir
          , "-DFFI_LIB_DIR="     ++ show ffiLibraryDir
          , "-DFFI_LIB="         ++ show libffiName ]

        , builder HsCpp ? flag HaveLibMingwEx ? arg "-DHAVE_LIBMINGWEX" ]

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
--  ld: absolute addressing (perhaps -mdynamic-no-pic) used in _stg_ap_0_fast
--      from rts/dist/build/Apply.dyn_o not allowed in slidable image
--
-- and lots of these warnings:
--
--  ld: warning codegen in _stg_ap_pppv_fast (offset 0x0000005E) prevents image
--      from loading in dyld shared cache
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
    i386   <- anyTargetArch ["i386"]
    goodOS <- not <$> anyTargetOs ["darwin", "solaris2"]
    return $ i386 && goodOS

-- See @rts/ghc.mk@.
rtsWarnings :: Args
rtsWarnings = mconcat
    [ arg "-Wall"
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

-- These numbers can be found at:
-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa383745(v=vs.85).aspx
-- If we're compiling on windows, enforce that we only support Vista SP1+
-- Adding this here means it doesn't have to be done in individual .c files
-- and also centralizes the versioning.
-- | Minimum supported Windows version.
windowsVersion :: String
windowsVersion = "0x06000100"
