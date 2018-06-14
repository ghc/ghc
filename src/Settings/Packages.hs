module Settings.Packages (packageArgs) where

import Expression
import Flavour
import GHC.Packages
import Oracles.Setting
import Oracles.Flag
import Rules.Gmp
import Settings

-- TODO: Finish migration of package-specific settings into a single file.
packageArgs :: Args
packageArgs = do
    intLib            <- getIntegerPackage
    stage             <- getStage
    rtsWays           <- getRtsWays
    path              <- getBuildPath
    compilerBuildPath <- expr $ buildPath (vanillaContext stage compiler)
    gmpBuildPath      <- expr gmpBuildPath

    let includeGmp = "-I" ++ gmpBuildPath -/- "include"

    mconcat
        --------------------------------- base ---------------------------------
        [ package base ? mconcat
          [ builder CabalFlags ? arg ('+' : pkgName intLib)

          -- This fixes the 'unknown symbol stat' issue.
          -- See: https://github.com/snowleopard/hadrian/issues/259.
          , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

        ------------------------------ bytestring ------------------------------
        , package bytestring ?
          builder CabalFlags ? intLib == integerSimple ? arg "integer-simple"

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
              pure ["-O0", "-fno-ignore-interface-pragmas", "-fcmm-sink" ] ]

          , builder (GhcCabal Conf) ? mconcat
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

          , builder CabalFlags ? mconcat
            [ ghcWithNativeCodeGen ? arg "ncg"
            , ghcWithInterpreter ? notStage0 ? arg "ghci"
            , flag CrossCompiling ? arg "-terminfo" ]

          , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]

        ---------------------------------- ghc ---------------------------------
        , package ghc ? mconcat
          [ builder Ghc ? arg ("-I" ++ compilerBuildPath)

          , builder CabalFlags ? mconcat
            [ ghcWithInterpreter ? notStage0 ? arg "ghci"
            , flag CrossCompiling ? arg "-terminfo" ] ]

        -------------------------------- ghcPkg --------------------------------
        , package ghcPkg ?
          builder CabalFlags ? flag CrossCompiling ? arg "-terminfo"

        -------------------------------- ghcPrim -------------------------------
        , package ghcPrim ? mconcat
          [ builder CabalFlags ? arg "include-ghc-prim"

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
          [ notStage0 ? builder CabalFlags ? arg "ghci"
          , flag CrossCompiling ? stage0 ? builder CabalFlags ? arg "ghci" ]

        -------------------------------- haddock -------------------------------
        , package haddock ?
          builder CabalFlags ? arg "in-ghc-tree"

        ------------------------------- haskeline ------------------------------
        , package haskeline ?
          builder CabalFlags ? flag CrossCompiling ? arg "-terminfo"

        -------------------------------- hsc2hs --------------------------------
        , package hsc2hs ?
          builder CabalFlags ? arg "in-ghc-tree"

        ------------------------------ integerGmp ------------------------------
        , package integerGmp ? mconcat
          [ builder Cc ? arg includeGmp

          , builder (GhcCabal Conf) ? mconcat
            [ -- TODO: This should respect some settings flag "InTreeGmp".
              -- Depending on @IncludeDir@ and @LibDir@ is bound to fail, since
              -- these are only set if the configure script was explicilty
              -- called with GMP include and lib dirs. Their absense as such
              -- does not imply @in-tree-gmp@.
              -- (null gmpIncludeDir && null gmpLibDir) ?
              -- arg "--configure-option=--with-intree-gmp"
              arg ("--configure-option=CFLAGS=" ++ includeGmp)
            , arg ("--gcc-options="             ++ includeGmp) ] ]

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
          builder CabalFlags ? notStage0 ? intLib == integerSimple ?
          pure [ "+integer-simple", "-bytestring-builder"]

        -------------------------------- runGhc --------------------------------
        , package runGhc ?
          builder Ghc ? input "//Main.hs" ?
          (\version -> ["-cpp", "-DVERSION=" ++ show version]) <$> getSetting ProjectVersion

        ---------------------------------- rts ---------------------------------
        , package rts ?
          builder CabalFlags ? (any (wayUnit Profiling) rtsWays) ? arg "profiling" ]
