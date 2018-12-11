module Rules.Program (buildProgramRules) where

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
buildProgramRules :: [(Resource, Int)] -> Rules ()
buildProgramRules rs = do
    root <- buildRootRules
    forM_ [Stage0 ..] $ \stage ->
        [ root -/- stageString stage -/- "bin"     -/- "*"
        , root -/- stageString stage -/- "lib/bin" -/- "*" ] |%> \bin -> do
            programContexts <- getProgramContexts stage
            case lookupProgramContext bin programContexts of
                Nothing  -> error $ "Unknown program " ++ show bin
                Just ctx -> buildProgram bin ctx rs

getProgramContexts :: Stage -> Action [(FilePath, Context)]
getProgramContexts stage = do
  -- This is quite inefficient, but we can't access 'programName' from
  -- 'Rules', because it is an 'Action' depending on an oracle.
  sPackages <- filter isProgram <$> stagePackages stage
  tPackages <- testsuitePackages
  -- TODO: Shall we use Stage2 for testsuite packages instead?
  let allPackages = sPackages
                ++ if stage == Stage1 then tPackages else []
  fmap concat . forM allPackages $ \pkg -> do
    -- the iserv pkg results in three different programs at
    -- the moment, ghc-iserv (built the vanilla way),
    -- ghc-iserv-prof (built the profiling way), and
    -- ghc-iserv-dyn (built the dynamic way).
    -- The testsuite requires all to be present, so we
    -- make sure that we cover these
    -- "prof-build-under-other-name" cases.
    -- iserv gets its names from Packages.hs:programName
    let allCtxs = [ vanillaContext stage pkg
                  , Context stage pkg profiling
                  -- TODO Dynamic way has been reverted as the dynamic build is
                  --      broken. See #15837.
                  -- , Context stage pkg dynamic
                  ]
    forM allCtxs $ \ctx -> do
      name <- programName ctx
      return (name <.> exe, ctx)

lookupProgramContext :: FilePath -> [(FilePath, Context)] -> Maybe Context
lookupProgramContext wholePath progs = lookup (takeFileName wholePath) progs

buildProgram :: FilePath -> Context -> [(Resource, Int)] -> Action ()
buildProgram bin ctx@(Context{..}) rs = do
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
