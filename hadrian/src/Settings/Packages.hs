module Settings.Packages (packageArgs) where

import Expression
import Flavour
import Oracles.Setting
import Oracles.Flag
import Packages
import Settings
import Settings.Builders.Common (wayCcArgs)

import qualified GHC.Toolchain.Library as Lib
import GHC.Toolchain.Target
import GHC.Platform.ArchOS
import Data.Version.Extra
import Settings.Program (ghcWithInterpreter)

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
        haveCurses = any (/= "") <$> traverse setting [ CursesIncludeDir, CursesLibDir ]

        -- Check if the bootstrap compiler has the same version as the one we
        -- are building. This is used to build cross-compilers
        bootCross = (==) <$> ghcVersionStage (stage0InTree) <*> ghcVersionStage Stage1

        compilerStageOption f = buildingCompilerStage' . f =<< expr flavour

    cursesIncludeDir <- getSetting CursesIncludeDir
    cursesLibraryDir <- getSetting CursesLibDir
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    libzstdIncludeDir <- getSetting LibZstdIncludeDir
    libzstdLibraryDir <- getSetting LibZstdLibDir
    stageVersion <- readVersion <$> (expr $ ghcVersionStage stage)

    mconcat
        --------------------------------- base ---------------------------------
        [ package base ? mconcat
          [ -- This fixes the 'unknown symbol stat' issue.
            -- See: https://github.com/snowleopard/hadrian/issues/259.
            builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

        --------------------------------- cabal --------------------------------
        -- Cabal is a large library and slow to compile. Moreover, we build it
        -- for Stage0 only so we can link ghc-pkg against it, so there is little
        -- reason to spend the effort to optimise it.
        , package cabal ?
          andM [stage0, notCross] ? builder Ghc ? arg "-O0"

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
            , inputs ["**/GHC/Hs/Instances.hs", "**/GHC/Driver/Session.hs"] ? andM [stage0, notCross] ?
              pure ["-O0"] ]

          , builder (Cabal Setup) ? mconcat
            [ anyTargetOs [OSOpenBSD] ? arg "--ld-options=-E"
            , compilerStageOption ghcProfiled ? arg "--ghc-pkg-option=--force"
            , cabalExtraDirs libzstdIncludeDir libzstdLibraryDir
            ]

          , builder (Cabal Flags) ? mconcat
            -- In order to enable internal-interpreter for the ghc
            -- library:
            --
            -- 1. ghcWithInterpreter must be True ("Use interpreter" =
            --    "YES")
            -- 2. For non-cross case it can be enabled
            -- 3. For cross case, disable for stage0 since that runs
            --    on the host and must rely on external interpreter to
            --    load target code, otherwise enable for stage1 since
            --    that runs on the target and can use target's own
            --    ghci object linker
            [ andM [expr (ghcWithInterpreter stage), orM [notCross, stage1]] `cabalFlag` "internal-interpreter"
            , orM [ notM cross, haveCurses ]  `cabalFlag` "terminfo"
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
            [ (expr (ghcWithInterpreter stage)) `cabalFlag` "internal-interpreter"
            , notStage0 `cabalFlag` "ghc-debug"
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
          builder (Cabal Flags) ? orM [ notM cross, haveCurses ] `cabalFlag` "terminfo"

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
        -- dependencies unless the user provided ncurses explicitly.
        -- TODO: Perhaps the user should be able to explicitly enable/disable this.
        , package haskeline ?
          builder (Cabal Flags) ? orM [ notM cross, haveCurses ] `cabalFlag` "terminfo"

        -------------------------------- terminfo ------------------------------
        , package terminfo ?
          builder (Cabal Setup) ? cabalExtraDirs cursesIncludeDir cursesLibraryDir

        -------------------------------- hsc2hs --------------------------------
        , package hsc2hs ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ------------------------------ ghc-internal ------------------------------
        , ghcInternalArgs

        ---------------------------------- rts ---------------------------------
        , package rts ? rtsPackageArgs -- RTS deserves a separate function
        , package libffi ? libffiPackageArgs

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

ghcInternalArgs :: Args
ghcInternalArgs = package ghcInternal ? do
    -- These are only used for non-in-tree builds.
    librariesGmp <- getSetting GmpLibDir
    includesGmp <- getSetting GmpIncludeDir

    backend <- getBignumBackend
    check   <- getBignumCheck

    mconcat
          [ -- select bignum backend
            builder (Cabal Flags) ? arg ("bignum-" <> backend)

          , -- check the selected backend against native backend
            builder (Cabal Flags) ? check `cabalFlag` "bignum-check"

            -- backend specific
          , case backend of
               "gmp" -> mconcat
                   [ builder (Cabal Setup) ? mconcat

                       -- enable GMP backend: configure script will produce
                       -- `ghc-internal.buildinfo` and `include/HsIntegerGmp.h`
                     [ arg "--configure-option=--with-gmp"

                       -- enable in-tree support: don't depend on external "gmp"
                       -- library
                     , flag GmpInTree ? arg "--configure-option=--with-intree-gmp"

                       -- prefer framework over library (on Darwin)
                     , flag GmpFrameworkPref ?
                       arg "--configure-option=--with-gmp-framework-preferred"

                       -- Ensure that the ghc-internal package registration includes
                       -- knowledge of the system gmp's library and include directories.
                     , notM (flag GmpInTree) ? cabalExtraDirs includesGmp librariesGmp
                     ]
                  ]
               _ -> mempty

          , builder (Cabal Flags) ? flag NeedLibatomic `cabalFlag` "need-atomic"

          ]

-- libffi and rts have to have the same flavour configuration
libffiPackageArgs :: Args
libffiPackageArgs = package libffi ? do
    rtsWays <- getRtsWays
    mconcat
        [ builder (Cabal Flags) ? mconcat
          [ any (wayUnit Profiling) rtsWays `cabalFlag` "profiling"
          , any (wayUnit Debug) rtsWays     `cabalFlag` "debug"
          , any (wayUnit Dynamic) rtsWays   `cabalFlag` "dynamic"
          , any (wayUnit Threaded) rtsWays  `cabalFlag` "threaded"
          ]
        ]

-- | RTS-specific command line arguments.
rtsPackageArgs :: Args
rtsPackageArgs = package rts ? do
    ghcUnreg       <- queryTarget tgtUnregisterised
    ghcEnableTNC   <- queryTarget tgtTablesNextToCode
    rtsWays        <- getRtsWays
    way            <- getWay
    path           <- getBuildPath
    top            <- expr topDirectory
    useSystemFfi   <- getFlag UseSystemFfi
    libdwIncludeDir   <- queryTarget (Lib.includePath <=< tgtRTSWithLibdw)
    libdwLibraryDir   <- queryTarget (Lib.libraryPath <=< tgtRTSWithLibdw)
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
                                                    , "-Og" ]
          -- Set the namespace for the rts fs functions
          , arg $ "-DFS_NAMESPACE=rts"

          , arg $ "-DCOMPILING_RTS"

          , inputs ["**/RtsMessages.c", "**/Trace.c"] ?
            pure
              [ "-DRtsWay=\"rts_" ++ show way ++ "\""
              ]

          , input "**/RtsUtils.c" ? pure
            [ "-DRtsWay=\"rts_" ++ show way ++ "\""
            ]

          -- We're after pure performance here. So make sure fast math and
          -- vectorization is enabled.
          , input "**/Hash.c" ? pure [ "-O3" ]

          , inputs ["**/Evac.c", "**/Evac_thr.c"] ? arg "-funroll-loops"

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

          , input "**/prim/atomic.c"  ? (not <$> flag CcLlvmBackend) ?
            arg "-Wno-sync-nand"
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
          , useLibdw                        `cabalFlag` "libdw"
          , flag UseLibnuma                 `cabalFlag` "libnuma"
          , flag UseLibzstd                 `cabalFlag` "libzstd"
          , flag StaticLibzstd              `cabalFlag` "static-libzstd"
          , queryTargetTarget tgtSymbolsHaveLeadingUnderscore `cabalFlag` "leading-underscore"
          , ghcUnreg                        `cabalFlag` "unregisterised"
          , ghcEnableTNC                    `cabalFlag` "tables-next-to-code"
          , Debug `wayUnit` way             `cabalFlag` "find-ptr"
          ]
        , builder (Cabal Setup) ? mconcat
              [ useLibdw ? cabalExtraDirs (fromMaybe "" libdwIncludeDir) (fromMaybe "" libdwLibraryDir)
              , cabalExtraDirs libnumaIncludeDir libnumaLibraryDir
              , cabalExtraDirs libzstdIncludeDir libzstdLibraryDir
              ]
        , builder (Cc (FindCDependencies CDep)) ? cArgs
        , builder (Cc (FindCDependencies  CxxDep)) ? cArgs
        , builder (Cc (FindCDependencies AsmDep)) ? cArgs
        , builder (Ghc CompileCWithGhc) ? map ("-optc" ++) <$> cArgs
        , builder (Ghc CompileCppWithGhc) ? map ("-optcxx" ++) <$> cArgs
        , builder Ghc ? ghcArgs

        , builder HsCpp ? pure
          [ "-DTOP="             ++ show top ]

        , builder HsCpp ? useLibdw ? arg "-DUSE_LIBDW" ]

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
