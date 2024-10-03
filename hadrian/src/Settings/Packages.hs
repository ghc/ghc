module Settings.Packages (packageArgs) where

import Expression
import Flavour
import Oracles.Setting
import Oracles.Flag
import Packages
import Settings
import Settings.Builders.Common (wayCcArgs)

import GHC.Toolchain.Target
import GHC.Platform.ArchOS
import Data.Version.Extra

-- | Package-specific command-line arguments.
packageArgs :: Args
packageArgs = do
    stage        <- getStage
    path         <- getBuildPath
    compilerPath <- expr $ buildPath (vanillaContext stage compiler)

    let -- Do not bind the result to a Boolean: this forces the configure rule
        -- immediately and may lead to cyclic dependencies.
        -- See: https://gitlab.haskell.org/ghc/ghc/issues/16809.
        cross = flag CrossCompiling

        -- Check if the bootstrap compiler has the same version as the one we
        -- are building. This is used to build cross-compilers
        bootCross = (==) <$> ghcVersionStage (stage0InTree) <*> ghcVersionStage Stage1

        compilerStageOption f = buildingCompilerStage' . f =<< expr flavour

    cursesIncludeDir <- getSetting CursesIncludeDir
    cursesLibraryDir <- getSetting CursesLibDir
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    stageVersion <- readVersion <$> (expr $ ghcVersionStage stage)

    mconcat
        --------------------------------- base ---------------------------------
        [ package base ? mconcat
          [ builder (Cabal Flags) ? notStage0 `cabalFlag` (pkgName ghcBignum)

          -- This fixes the 'unknown symbol stat' issue.
          -- See: https://github.com/snowleopard/hadrian/issues/259.
          , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

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
            [ compilerStageOption ghcDebugAssertions ? arg "-DDEBUG"

            , inputs ["**/GHC.hs", "**/GHC/Driver/Make.hs"] ? arg "-fprof-auto"
            , input "**/Parser.hs" ?
              pure ["-fno-ignore-interface-pragmas", "-fcmm-sink"]
            -- Enable -haddock and -Winvalid-haddock for the compiler
            , arg "-haddock"
            , notStage0 ? arg "-Winvalid-haddock"
            -- These files take a very long time to compile with -O1,
            -- so we use -O0 for them just in Stage0 to speed up the
            -- build but not affect Stage1+ executables
            , inputs ["**/GHC/Hs/Instances.hs", "**/GHC/Driver/Session.hs"] ? stage0 ?
              pure ["-O0"] ]

          , builder (Cabal Setup) ? mconcat
            [ arg "--disable-library-for-ghci"
            , anyTargetOs [OSOpenBSD] ? arg "--ld-options=-E"
            , compilerStageOption ghcProfiled ? arg "--ghc-pkg-option=--force" ]

          , builder (Cabal Flags) ? mconcat
            -- For the ghc library, internal-interpreter only makes
            -- sense when we're not cross compiling. For cross GHC,
            -- external interpreter is used for loading target code
            -- and internal interpreter is supposed to load native
            -- code for plugins (!7377), however it's unfinished work
            -- (#14335) and completely untested in CI for cross
            -- backends at the moment, so we might as well disable it
            -- for cross GHC.
            [ andM [expr ghcWithInterpreter, notStage0, notCross] `cabalFlag` "internal-interpreter"
            , notM cross `cabalFlag` "terminfo"
            , arg "-build-tool-depends"
            , flag UseLibzstd `cabalFlag` "with-libzstd"
            -- ROMES: While the boot compiler is not updated wrt -this-unit-id
            -- not being fixed to `ghc`, when building stage0, we must set
            -- -this-unit-id to `ghc` because the boot compiler expects that.
            -- We do it through a cabal flag in ghc.cabal
            , stageVersion < makeVersion [9,8,1] ? arg "+hadrian-stage0"
            , flag StaticLibzstd `cabalFlag` "static-libzstd"
            , stage0 `cabalFlag` "bootstrap"
            ]

          , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]

        ---------------------------------- ghc ---------------------------------
        , package ghc ? mconcat
          [ builder Ghc ? mconcat
             [ arg ("-I" ++ compilerPath)
             , compilerStageOption ghcDebugAssertions ? arg "-DDEBUG" ]

          , builder (Cabal Flags) ? mconcat
            [ andM [expr ghcWithInterpreter, notStage0] `cabalFlag` "internal-interpreter"
            , ifM stage0
                  -- We build a threaded stage 1 if the bootstrapping compiler
                  -- supports it.
                  (threadedBootstrapper `cabalFlag` "threaded")

                  -- We build a threaded stage N, N>1 if the configuration calls
                  -- for it.
                  (compilerStageOption ghcThreaded `cabalFlag` "threaded")
            ]
          ]

        -------------------------------- ghcPkg --------------------------------
        , package ghcPkg ?
          builder (Cabal Flags) ? notM cross `cabalFlag` "terminfo"

        -------------------------------- ghcPrim -------------------------------
        , package ghcPrim ? mconcat
          [ builder (Cabal Flags) ? flag NeedLibatomic `cabalFlag` "need-atomic"

          , builder (Cc CompileC) ? (not <$> flag CcLlvmBackend) ?
            input "**/cbits/atomic.c"  ? arg "-Wno-sync-nand" ]

        -------------------------------- ghcBoot ------------------------------
        , package ghcBoot ?
            builder (Cabal Flags) ? (stage0 `cabalFlag` "bootstrap")

        --------------------------------- ghci ---------------------------------
        , package ghci ? mconcat
          [
          -- The use case here is that we want to build @iserv-proxy@ for the
          -- cross compiler. That one needs to be compiled by the bootstrap
          -- compiler as it needs to run on the host. Hence @iserv@ needs
          -- @GHCi.TH@, @GHCi.Message@, @GHCi.Run@, and @GHCi.Server@ from
          -- @ghci@. And those are behind the @-finternal-interpreter@ flag.
          --
          -- But it may not build if we have made some changes to ghci's
          -- dependencies (see #16051).
          --
          -- To fix this properly Hadrian would need to:
          --   * first build a compiler for the build platform (stage1 is enough)
          --   * use it as a bootstrap compiler to build the stage1 cross-compiler
          --
          -- The issue is that "configure" would have to be executed twice (for
          -- the build platform and for the cross-platform) and Hadrian would
          -- need to be fixed to support two different stage1 compilers.
          --
          -- The workaround we use is to check if the bootstrap compiler has
          -- the same version as the one we are building. In this case we can
          -- avoid the first step above and directly build with
          -- `-finternal-interpreter`.
          --
          -- TODO: Note that in that case we also do not need to build most of
          -- the Stage1 libraries, as we already know that the bootstrap
          -- compiler comes with the same versions as the one we are building.
          --
            builder (Cabal Setup) ? cabalExtraDirs ffiIncludeDir ffiLibraryDir
          , builder (Cabal Flags) ? mconcat
            [ ifM stage0
                (andM [cross, bootCross] `cabalFlag` "internal-interpreter")
                (arg "internal-interpreter")
            , stage0 `cabalFlag` "bootstrap"
            ]

          ]

        , package unix ? builder (Cabal Flags) ? arg "+os-string"
        , package directory ? builder (Cabal Flags) ? arg "+os-string"
        , package win32 ? builder (Cabal Flags) ? arg "+os-string"

        --------------------------------- iserv --------------------------------
        -- Add -Wl,--export-dynamic enables GHCi to load dynamic objects that
        -- refer to the RTS.  This is harmless if you don't use it (adds a bit
        -- of overhead to startup and increases the binary sizes) but if you
        -- need it there's no alternative.
        --
        -- The Solaris linker does not support --export-dynamic option. It also
        -- does not need it since it exports all dynamic symbols by default
        , package iserv
          ? expr isElfTarget
          ? notM (expr $ anyTargetOs [OSFreeBSD, OSSolaris2])? mconcat
          [ builder (Ghc LinkHs) ? arg "-optl-Wl,--export-dynamic" ]

        -------------------------------- haddock -------------------------------
        , package haddockApi ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ---------------------------- ghc-boot-th-next --------------------------
        , package ghcBootThNext ?
            builder (Cabal Flags) ? stage0 `cabalFlag` "bootstrap"

        ---------------------------------- text --------------------------------
        , package text ?
            ifM (textWithSIMDUTF <$> expr flavour)
              (builder (Cabal Flags) ? arg "+simdutf")
              (builder (Cabal Flags) ? arg "-simdutf")

        ------------------------------- haskeline ------------------------------
        -- Hadrian doesn't currently support packages containing both libraries
        -- and executables. This flag disables the latter.
        , package haskeline ?
          builder (Cabal Flags) ? arg "-examples"
        -- Don't depend upon terminfo when cross-compiling to avoid unnecessary
        -- dependencies.
        -- TODO: Perhaps the user should rather be responsible for this?
        , package haskeline ?
          builder (Cabal Flags) ? notM cross `cabalFlag` "terminfo"

        -------------------------------- terminfo ------------------------------
        , package terminfo ?
          builder (Cabal Setup) ? cabalExtraDirs cursesIncludeDir cursesLibraryDir

        -------------------------------- hsc2hs --------------------------------
        , package hsc2hs ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ------------------------------ ghc-bignum ------------------------------
        , ghcBignumArgs

        ---------------------------------- rts ---------------------------------
        , package rts ? rtsPackageArgs -- RTS deserves a separate function

        -------------------------------- runGhc --------------------------------
        , package runGhc ?
          builder Ghc ? input "**/Main.hs" ?
          (\version -> ["-cpp", "-DVERSION=" ++ show version]) <$> getSetting ProjectVersion

        --------------------------------- genprimopcode ------------------------
        , package genprimopcode
          ? builder (Cabal Flags) ? arg "-build-tool-depends"

        --------------------------------- hpcBin ----------------------------------
        , package hpcBin
          ? builder (Cabal Flags) ? arg "-build-tool-depends"

        ]

ghcBignumArgs :: Args
ghcBignumArgs = package ghcBignum ? do
    -- These are only used for non-in-tree builds.
    librariesGmp <- getSetting GmpLibDir
    includesGmp <- getSetting GmpIncludeDir

    backend <- getBignumBackend
    check   <- getBignumCheck

    mconcat
          [ -- select BigNum backend
            builder (Cabal Flags) ? arg backend

          , -- check the selected backend against native backend
            builder (Cabal Flags) ? check `cabalFlag` "check"

            -- backend specific
          , case backend of
               "gmp" -> mconcat
                   [ builder (Cabal Setup) ? mconcat

                       -- enable GMP backend: configure script will produce
                       -- `ghc-bignum.buildinfo` and `include/HsIntegerGmp.h`
                     [ arg "--configure-option=--with-gmp"

                       -- enable in-tree support: don't depend on external "gmp"
                       -- library
                     , flag GmpInTree ? arg "--configure-option=--with-intree-gmp"

                       -- prefer framework over library (on Darwin)
                     , flag GmpFrameworkPref ?
                       arg "--configure-option=--with-gmp-framework-preferred"

                       -- Ensure that the ghc-bignum package registration includes
                       -- knowledge of the system gmp's library and include directories.
                     , notM (flag GmpInTree) ? cabalExtraDirs includesGmp librariesGmp
                     ]
                  ]
               _ -> mempty
          ]

-- | RTS-specific command line arguments.
rtsPackageArgs :: Args
rtsPackageArgs = package rts ? do
    projectVersion <- getSetting ProjectVersion
    hostPlatform   <- queryHost targetPlatformTriple
    hostArch       <- queryHost queryArch
    hostOs         <- queryHost queryOS
    hostVendor     <- queryHost queryVendor
    buildPlatform  <- queryBuild targetPlatformTriple
    buildArch      <- queryBuild queryArch
    buildOs        <- queryBuild queryOS
    buildVendor    <- queryBuild queryVendor
    targetPlatform <- queryTarget targetPlatformTriple
    targetArch     <- queryTarget queryArch
    targetOs       <- queryTarget queryOS
    targetVendor   <- queryTarget queryVendor
    ghcUnreg       <- queryTarget tgtUnregisterised
    ghcEnableTNC   <- queryTarget tgtTablesNextToCode
    rtsWays        <- getRtsWays
    way            <- getWay
    path           <- getBuildPath
    top            <- expr topDirectory
    useSystemFfi   <- getFlag UseSystemFfi
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    libdwIncludeDir   <- getSetting LibdwIncludeDir
    libdwLibraryDir   <- getSetting LibdwLibDir
    libnumaIncludeDir <- getSetting LibnumaIncludeDir
    libnumaLibraryDir <- getSetting LibnumaLibDir
    libzstdIncludeDir <- getSetting LibZstdIncludeDir
    libzstdLibraryDir <- getSetting LibZstdLibDir

    x86 <- queryTarget (\ tgt -> archOS_arch (tgtArchOs tgt) `elem` [ ArchX86, ArchX86_64 ])

    -- Arguments passed to GHC when compiling C and .cmm sources.
    let ghcArgs = mconcat
          [ arg "-Irts"
          , arg $ "-I" ++ path
          , way `elem` [debug, debugDynamic] ? pure [ "-DTICKY_TICKY"
                                                    , "-optc-DTICKY_TICKY"]
          , Profiling `wayUnit` way          ? arg "-DPROFILING"
          , Threaded  `wayUnit` way          ? arg "-DTHREADED_RTS"
          , notM targetSupportsSMP           ? arg "-optc-DNOSMP"

            -- See Note [AutoApply.cmm for vectors] in genapply/Main.hs
            --
            -- In particular, we **do not** pass -mavx when compiling
            -- AutoApply_V16.cmm, as that would lock out targets with SSE2 but not AVX.
          , inputs ["**/AutoApply_V32.cmm"] ? pure [ "-mavx2"    | x86 ]
          , inputs ["**/AutoApply_V64.cmm"] ? pure [ "-mavx512f" | x86 ]

          , inputs ["**/Jumps_V32.cmm"] ? pure [ "-mavx2"    | x86 ]
          , inputs ["**/Jumps_V64.cmm"] ? pure [ "-mavx512f" | x86 ]
          ]

    let cArgs = mconcat
          [ rtsWarnings
          , wayCcArgs
          , arg "-fomit-frame-pointer"
          -- RTS *must* be compiled with optimisations. The INLINE_HEADER macro
          -- requires that functions are inlined to work as expected. Inlining
          -- only happens for optimised builds. Otherwise we can assume that
          -- there is a non-inlined variant to use instead. But RTS does not
          -- provide non-inlined alternatives and hence needs the function to
          -- be inlined. See https://github.com/snowleopard/hadrian/issues/90.
          , arg "-O2"

          , arg "-Irts"
          , arg $ "-I" ++ path

          , notM targetSupportsSMP           ? arg "-DNOSMP"

          , Debug     `wayUnit` way          ? pure [ "-DDEBUG"
                                                    , "-fno-omit-frame-pointer"
                                                    , "-g3"
                                                    , "-O0" ]
          -- Set the namespace for the rts fs functions
          , arg $ "-DFS_NAMESPACE=rts"

          , arg $ "-DCOMPILING_RTS"

          , inputs ["**/RtsMessages.c", "**/Trace.c"] ?
            pure
              ["-DProjectVersion=" ++ show projectVersion
              , "-DRtsWay=\"rts_" ++ show way ++ "\""
              ]

          , input "**/RtsUtils.c" ? pure
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
            , "-DGhcUnregisterised="         ++ show (yesNo ghcUnreg)
            , "-DTablesNextToCode="          ++ show (yesNo ghcEnableTNC)
            , "-DRtsWay=\"rts_" ++ show way ++ "\""
            ]

          -- We're after pure performance here. So make sure fast math and
          -- vectorization is enabled.
          , input "**/Hash.c" ? pure [ "-O3" ]

          , inputs ["**/Evac.c", "**/Evac_thr.c"] ? arg "-funroll-loops"

          , speedHack ?
            inputs [ "**/Evac.c", "**/Evac_thr.c"
                   , "**/Scav.c", "**/Scav_thr.c"
                   , "**/Compact.c", "**/GC.c" ] ? arg "-fno-PIC"
          -- @-static@ is necessary for these bits, as otherwise the NCG
          -- generates dynamic references.
          , speedHack ?
            inputs [ "**/Updates.c", "**/StgMiscClosures.c"
                   , "**/Jumps_D.c", "**/Jumps_V16.c", "**/Jumps_V32.c", "**/Jumps_V64.c"
                   , "**/PrimOps.c", "**/Apply.c"
                   , "**/AutoApply.c"
                   , "**/AutoApply_V16.c"
                   , "**/AutoApply_V32.c"
                   , "**/AutoApply_V64.c" ] ? pure ["-fno-PIC", "-static"]

            -- See Note [AutoApply.cmm for vectors] in genapply/Main.hs
          , inputs ["**/AutoApply_V32.c"] ? pure [ "-mavx2"    | x86 ]
          , inputs ["**/AutoApply_V64.c"] ? pure [ "-mavx512f" | x86 ]

          , inputs ["**/Jumps_V32.c"] ? pure [ "-mavx2"    | x86 ]
          , inputs ["**/Jumps_V64.c"] ? pure [ "-mavx512f" | x86 ]

          -- inlining warnings happen in Compact
          , inputs ["**/Compact.c"] ? arg "-Wno-inline"

          -- emits warnings about call-clobbered registers on x86_64
          , inputs [ "**/StgCRun.c"
                   , "**/win32/ConsoleHandler.c", "**/win32/ThrIOManager.c"] ? arg "-w"
          -- The above warning suppression flags are a temporary kludge.
          -- While working on this module you are encouraged to remove it and fix
          -- any warnings in the module. See:
          -- https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions#Warnings

          , (not <$> flag CcLlvmBackend) ?
            inputs ["**/Compact.c"] ? arg "-finline-limit=2500"

          , input "**/RetainerProfile.c" ? flag CcLlvmBackend ?
            arg "-Wno-incompatible-pointer-types"
          ]

    mconcat
        [ builder (Cabal Flags) ? mconcat
          [ any (wayUnit Profiling) rtsWays `cabalFlag` "profiling"
          , any (wayUnit Debug) rtsWays     `cabalFlag` "debug"
          , any (wayUnit Dynamic) rtsWays   `cabalFlag` "dynamic"
          , any (wayUnit Threaded) rtsWays  `cabalFlag` "threaded"
          , flag UseLibm                    `cabalFlag` "libm"
          , flag UseLibrt                   `cabalFlag` "librt"
          , flag UseLibdl                   `cabalFlag` "libdl"
          , useSystemFfi                    `cabalFlag` "use-system-libffi"
          , useLibffiForAdjustors           `cabalFlag` "libffi-adjustors"
          , flag UseLibpthread              `cabalFlag` "need-pthread"
          , flag UseLibbfd                  `cabalFlag` "libbfd"
          , flag NeedLibatomic              `cabalFlag` "need-atomic"
          , flag UseLibdw                   `cabalFlag` "libdw"
          , flag UseLibnuma                 `cabalFlag` "libnuma"
          , flag UseLibzstd                 `cabalFlag` "libzstd"
          , flag StaticLibzstd              `cabalFlag` "static-libzstd"
          , queryTargetTarget tgtSymbolsHaveLeadingUnderscore `cabalFlag` "leading-underscore"
          , ghcUnreg                        `cabalFlag` "unregisterised"
          , ghcEnableTNC                    `cabalFlag` "tables-next-to-code"
          , Debug `wayUnit` way             `cabalFlag` "find-ptr"
          ]
        , builder (Cabal Setup) ? mconcat
              [ cabalExtraDirs libdwIncludeDir libdwLibraryDir
              , cabalExtraDirs libnumaIncludeDir libnumaLibraryDir
              , cabalExtraDirs libzstdIncludeDir libzstdLibraryDir
              , useSystemFfi ? cabalExtraDirs ffiIncludeDir ffiLibraryDir
              ]
        , builder (Cc (FindCDependencies CDep)) ? cArgs
        , builder (Cc (FindCDependencies  CxxDep)) ? cArgs
        , builder (Ghc CompileCWithGhc) ? map ("-optc" ++) <$> cArgs
        , builder (Ghc CompileCppWithGhc) ? map ("-optcxx" ++) <$> cArgs
        , builder Ghc ? ghcArgs

        , builder HsCpp ? pure
          [ "-DTOP="             ++ show top ]

        , builder HsCpp ? flag UseLibdw ? arg "-DUSE_LIBDW" ]

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
--      from rts/dist-install/build/Apply.dyn_o not allowed in slidable image
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
-- .rodata (section)                   0x11        rts/dist-install/build/Apply.dyn_o
--   ...
-- ld: fatal: relocations remain against allocatable but non-writable sections
-- collect2: ld returned 1 exit status
speedHack :: Action Bool
speedHack = do
    i386   <- anyTargetArch [ArchX86]
    goodOS <- not <$> anyTargetOs [OSSolaris2]
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
    , arg "-Wpointer-arith"
    , arg "-Wmissing-noreturn"
    , arg "-Wnested-externs"
    , arg "-Wredundant-decls"
    , arg "-Wundef"
    , arg "-fno-strict-aliasing" ]

-- | Expands to Cabal `--extra-lib-dirs` and `--extra-include-dirs` flags if
-- the respective paths are not null.
cabalExtraDirs :: FilePath   -- ^ include path
          -> FilePath   -- ^ libraries path
          -> Args
cabalExtraDirs include lib = mconcat
    [ extraDirFlag "--extra-lib-dirs" lib
    , extraDirFlag "--extra-include-dirs" include
    ]
  where
    extraDirFlag :: String -> FilePath -> Args
    extraDirFlag flag dir
      | null dir  = mempty
      | otherwise = arg (flag++"="++dir)
