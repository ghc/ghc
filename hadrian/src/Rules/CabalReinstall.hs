module Rules.CabalReinstall where

import Context
import Expression
import Oracles.Flag
import Packages
import Settings
import Target
import Utilities
import qualified System.Directory.Extra as IO
import Data.Either
import Rules.BinaryDist
import Oracles.Setting
import BindistConfig

{-
Note [Testing reinstallable GHC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
To test the reinstallable GHC configuration, we install a GHC to <build root>/stage-cabal/bin
along with appropriate wrapper scripts.

The libdir of the reinstalled GHC points to the libdir of the stage 2 compiler (in <build root>/stage1)
-}


-- | We don't support reinstalling these
cabalExcludedPackages :: [Package]
cabalExcludedPackages = [array, base, deepseq, filepath, ghcBootTh, pretty, templateHaskell]


cabalBuildRules :: Rules ()
cabalBuildRules = do
    root <- buildRootRules
    root -/- "stage-cabal" -/- "cabal-packages" %> \outpath -> do
      -- Always rerun to pass onto cabal's own recompilation logic
      alwaysRerun
      all_pkgs <- stagePackages Stage1
      forM_ (filter (not . (`elem` cabalExcludedPackages)) all_pkgs) $ \pkg -> do
        withVerbosity Diagnostic $
          buildWithCmdOptions [] $
            target (vanillaContext Stage2 pkg) (Cabal Install Stage2) [] []
      liftIO $ writeFile outpath "done"

    phony "build-cabal" $ need [root -/- "stage-cabal" -/- "bin" -/- ".stamp"]

    root -/- "stage-cabal" -/- "bin" -/- "*" %> \_ -> need [root -/- "stage-cabal" -/- "bin" -/- ".stamp"]

    priority 2.0 $ root -/- "stage-cabal" -/- "bin" -/- ".stamp" %> \stamp -> do
        -- We 'need' all binaries and libraries
        all_pkgs <- stagePackages Stage1
        (lib_targets, bin_targets) <- partitionEithers <$> mapM (pkgTarget normalBindist) all_pkgs
        cross <- flag CrossCompiling
        iserv_targets <- if cross then pure [] else iservBins
        need (map snd (lib_targets ++ bin_targets ++ iserv_targets))

        distDir        <- Context.distDir (vanillaContext Stage1 rts)
        let rtsIncludeDir    = distDir -/- "include"

        libdir  <- liftIO . makeAbsolute =<< stageLibPath Stage1
        work_dir <- liftIO $ makeAbsolute $ root -/- "stage-cabal"
        let outputDir = work_dir -/- "bin"
        includeDir <- liftIO $ makeAbsolute rtsIncludeDir

        createDirectory outputDir

        need [root -/- "stage-cabal" -/- "cabal-packages"]

        cwd <- liftIO $ IO.getCurrentDirectory
        version        <- setting ProjectVersion

        let cabal_package_db = cwd -/- root -/- "stage-cabal" -/- "dist-newstyle" -/- "packagedb" -/- "ghc-" ++ version

        forM_ (filter ((/= iserv) . fst) bin_targets) $ \(bin_pkg,_bin_path) -> do
            let pgmName pkg
                  | pkg == ghc    = "ghc"
                  | pkg == hpcBin = "hpc"
                  | otherwise     = pkgName pkg
            let cabal_bin_out = work_dir -/- "cabal-bin" -/- (pgmName bin_pkg)
            needed_wrappers <- pkgToWrappers Stage2 bin_pkg
            forM_ needed_wrappers $ \wrapper_name -> do
              let wrapper_prefix = unlines
                    ["#!/usr/bin/env sh"
                    ,"executablename="++show cabal_bin_out
                    ,"libdir="++show libdir
                    ,"bindir="++show outputDir
                    ,"exedir="++show outputDir
                    ,"includedir="++show includeDir
                    ,"export GHC_PACKAGE_PATH="++show cabal_package_db++":"
                    ]
                  output_file = outputDir -/- wrapper_name
              wrapper_content <- wrapper Stage2 wrapper_name
              writeFile' output_file (wrapper_prefix ++ wrapper_content)
              makeExecutable output_file
              pure ()

        -- Just symlink these for now
        -- TODO: build these with cabal as well
        forM_ iserv_targets $ \(_bin_pkg,bin_path') -> do
            bin_path <- liftIO $ makeAbsolute bin_path'
            let orig_filename = takeFileName bin_path
                output_file = outputDir -/- orig_filename
            liftIO $ do
              IO.removeFile output_file <|> pure ()
              IO.createFileLink bin_path output_file
            pure ()
        writeFile' stamp "OK"
