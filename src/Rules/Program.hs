module Rules.Program (buildProgram) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.PackageData as PD

import Base
import Context
import Expression hiding (stage, way)
import GHC
import Oracles.ModuleFiles
import Oracles.Flag (crossCompiling)
import Settings
import Settings.Packages.Rts
import Target
import Utilities

-- | TODO: Drop code duplication
buildProgram :: [(Resource, Int)] -> Rules ()
buildProgram rs = do
    root <- buildRootRules
    forM_ [Stage0 ..] $ \stage ->
        [ root -/- stageString stage -/- "bin"     -/- "*"
        , root -/- stageString stage -/- "lib/bin" -/- "*" ] |%> \bin -> do
            -- This is quite inefficient, but we can't access 'programName' from
            -- 'Rules', because it is an 'Action' depending on an oracle.
            sPackages <- filter isProgram <$> stagePackages stage
            tPackages <- testsuitePackages
            -- TODO: Shall we use Stage2 for testsuite packages instead?
            let allPackages = sPackages
                           ++ if stage == Stage1 then tPackages else []
            nameToCtxList <- forM allPackages $ \pkg -> do
                let ctx = vanillaContext stage pkg
                name <- programName ctx
                return (name <.> exe, ctx)

            case lookup (takeFileName bin) nameToCtxList of
                Nothing -> error $ "Unknown program " ++ show bin
                Just (Context {..}) -> do
                    -- Custom dependencies: this should be modeled better in the
                    -- Cabal file somehow.
                    -- TODO: Is this still needed? See 'runtimeDependencies'.
                    when (package == hsc2hs) $ do
                        -- 'Hsc2hs' needs the @template-hsc.h@ file.
                        template <- templateHscPath stage
                        need [template]
                    when (package == ghc) $ do
                        -- GHC depends on @settings@, @platformConstants@,
                        -- @llvm-targets@, @ghc-usage.txt@, @ghci-usage.txt@,
                        -- @llvm-passes@.
                        need =<< ghcDeps stage

                    cross <- crossCompiling
                    -- For cross compiler, copy @stage0/bin/<pgm>@ to @stage1/bin/@.
                    case (cross, stage) of
                        (True, s) | s > Stage0 -> do
                            srcDir <- buildRoot <&> (-/- (stageString Stage0 -/- "bin"))
                            copyFile (srcDir -/- takeFileName bin) bin
                        _ -> buildBinary rs bin =<< programContext stage package

buildBinary :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinary rs bin context@Context {..} = do
    binDeps <- if stage == Stage0 && package == ghcCabal
        then hsSources context
        else do
            needLibrary =<< contextDependencies context
            when (stage > Stage0) $ do
                ways <- interpretInContext context (getLibraryWays <> getRtsWays)
                needLibrary [ rtsContext { way = w } | w <- ways ]
            cSrcs  <- interpretInContext context (getPackageData PD.cSrcs)
            cObjs  <- mapM (objectPath context) cSrcs
            hsObjs <- hsObjects context
            return $ cObjs ++ hsObjs
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- pkgSynopsis context
    putSuccess $ renderProgram
        (quote (pkgName package) ++ " (" ++ show stage ++ ").") bin synopsis
