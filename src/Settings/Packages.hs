module Settings.Packages (packageArgs) where

import Expression
import Flavour
import GHC.Packages
import Oracles.Setting
import Oracles.Flag
import Rules.Gmp
import Settings

packageArgs :: Args
packageArgs = do
  intLibPkg <- getIntegerPackage
  integerLibraryName <- pkgName <$> getIntegerPackage

  stage   <- getStage
  rtsWays <- getRtsWays
  path    <- getBuildPath

  compilerBuildPath <- expr $ buildPath (vanillaContext stage compiler)

  gmpBuildPath <- expr gmpBuildPath
  let includeGmp = "-I" ++ gmpBuildPath -/- "include"

  mconcat
    [ package base
      ? mconcat [ builder CabalFlags ? arg ('+':integerLibraryName)
                  -- This fixes the 'unknown symbol stat' issue.
                  -- See: https://github.com/snowleopard/hadrian/issues/259.
                , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]
    , package bytestring
      ? builder CabalFlags ? intLibPkg == integerSimple ? arg "integer-simple"
    , package text
      -- text is rather tricky. It's a boot lib, and it tries to determine on
      -- it's own if it should link against integer-gmp or integer-simple.
      -- For stage0, we need to use the integer library that the bootstrap
      -- compiler has. (the interger-lib is not a boot lib) but as such, we'll
      -- copy it over into the stage0 package-db (maybe we should stop doing this?)
      -- And subsequently text for stage1 will detect the same integer lib again,
      -- even though we don't build it in stage1, and at that point the
      -- configuration is just wrong.
      ? builder CabalFlags ? notStage0 ? intLibPkg == integerSimple ? pure [ "+integer-simple"
                                                                           , "-bytestring-builder"]
    , package cabal
      -- Cabal is a rather large library and quite slow to compile. Moreover, we
      -- build it for stage0 only so we can link ghc-pkg against it, so there is
      -- little reason to spend the effort to optimize it.
      ? stage0 ? builder Ghc ? arg "-O0"
    , package compiler
      ? mconcat [ builder Alex ? arg "--latin1"
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
                  , ghcWithInterpreter ?
                    notStage0 ? arg "ghci"
                  , crossCompiling ? arg "-terminfo"
                  ]
                , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]
    , package ghc
      ? mconcat [ builder Ghc        ? arg ("-I" ++ compilerBuildPath)
                , builder CabalFlags ? ghcWithInterpreter ? notStage0 ? arg "ghci"
                , builder CabalFlags ? crossCompiling ? arg "-terminfo" ]
    , package ghcPkg
      ? builder CabalFlags ? crossCompiling ? arg "-terminfo"
    , package ghcPrim
      ? mconcat [ builder CabalFlags ? arg "include-ghc-prim"
                , builder (Cc CompileC)     ?
                  (not <$> flag GccIsClang) ?
                  input "//cbits/atomic.c"  ? arg "-Wno-sync-nand" ]
      -- XXX: This should not be *not <$> crossCompiling*, but ensure
      --      that the bootstrap compiler has the same version as the
      --      one we are building.
      -- XXX: In that case we also do not need to build most of the
      --      stage1 libraries, as we already know that the compiler
      --      comes with the most recent versions.
      -- XXX: The use case here is that we want to build ghc-proxy for
      --      the cross compiler. That one needs to be compiled by the
      --      bootstrap compiler as it needs to run on the host. and as
      --      such libiserv needs GHCi.TH, GHCi.Message and GHCi.Run from
      --      ghci. And those are beind the -fghci flag.
    , package ghci ? notStage0 ? builder CabalFlags ? arg "ghci"
    , package ghci ? crossCompiling ? stage0 ? builder CabalFlags ? arg "ghci"
    , package haddock ? builder CabalFlags ? arg "in-ghc-tree"
    , package haskeline ? builder CabalFlags ? crossCompiling ? arg "-terminfo"
    , package hsc2hs ? builder CabalFlags ? arg "in-ghc-tree"
    , package integerGmp
      ? mconcat [ builder Cc ? arg includeGmp
                , builder (GhcCabal Conf) ? mconcat
                  [ -- (null gmpIncludeDir && null gmpLibDir) ?
                    -- XXX: this should respect some settings flag "InTreeGmp".
                    --      depending on include and lib dir, is bound to fail
                    --      these are only set if ./configure was explicilty
                    --      called with gmp include and lib dirs.  Their absense
                    --      as such does not imply in-tree-gmp
                    -- arg "--configure-option=--with-intree-gmp"
                    arg ("--configure-option=CFLAGS=" ++ includeGmp)
                  , arg ("--gcc-options="             ++ includeGmp) ] ]
    , package runGhc
      ? builder Ghc
      ? input "//Main.hs"
      ? (\version -> ["-cpp", "-DVERSION=" ++ show version]) <$> getSetting ProjectVersion
    , package rts
      ? builder CabalFlags ? (any (wayUnit Profiling) rtsWays) ? arg "profiling"
    ]
