module Rules.Program (buildProgram) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type

import Base
import Context
import Expression hiding (stage, way)
import Oracles.Flag
import Oracles.ModuleFiles
import Packages
import Settings
import Settings.Default
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
            nameToCtxList <- fmap concat . forM allPackages $ \pkg -> do
                -- the iserv pkg results in two different programs at
                -- the moment, ghc-iserv (built the vanilla way)
                -- and ghc-iserv-prof (built the profiling way), and
                -- the testsuite requires both to be present, so we
                -- make sure that we cover these
                -- "prof-build-under-other-name" cases.
                -- iserv gets its two names from Packages.hs:programName
                let ctxV = vanillaContext stage pkg
                    ctxProf = Context stage pkg profiling
                nameV <- programName ctxV
                nameProf <- programName ctxProf
                return [ (nameV <.> exe, ctxV), (nameProf <.> exe, ctxProf) ]

            case lookup (takeFileName bin) nameToCtxList of
                Nothing -> error $ "Unknown program " ++ show bin
                Just ctx@(Context {..}) -> do
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

                    cross <- flag CrossCompiling
                    -- For cross compiler, copy @stage0/bin/<pgm>@ to @stage1/bin/@.
                    case (cross, stage) of
                        (True, s) | s > Stage0 -> do
                            srcDir <- buildRoot <&> (-/- (stageString Stage0 -/- "bin"))
                            copyFile (srcDir -/- takeFileName bin) bin
                        (False, s) | s > Stage0 && (package `elem` [touchy, unlit]) -> do
                            srcDir <- stageLibPath Stage0 <&> (-/- "bin")
                            copyFile (srcDir -/- takeFileName bin) bin
                        _ -> buildBinary rs bin ctx

buildBinary :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinary rs bin context@Context {..} = do
    needLibrary =<< contextDependencies context
    when (stage > Stage0) $ do
        ways <- interpretInContext context (getLibraryWays <> getRtsWays)
        needLibrary [ rtsContext { way = w } | w <- ways ]
    cSrcs  <- interpretInContext context (getContextData cSrcs)
    cObjs  <- mapM (objectPath context) cSrcs
    hsObjs <- hsObjects context
    let binDeps = cObjs ++ hsObjs
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- pkgSynopsis package
    putSuccess $ renderProgram
        (quote (pkgName package) ++ " (" ++ show stage ++ ").") bin synopsis
