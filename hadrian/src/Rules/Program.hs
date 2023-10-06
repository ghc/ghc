module Rules.Program (buildProgramRules) where

import qualified Data.Set as Set

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type

import Base
import Context
import Expression hiding (stage, way)
import Oracles.ModuleFiles
import Oracles.Setting (topDirectory)
import Packages
import Settings
import Settings.Default
import Settings.Program (programContext)
import Target
import Utilities
import Rules.Library
import Rules.Register

-- | TODO: Drop code duplication
buildProgramRules :: [(Resource, Int)] -> Rules ()
buildProgramRules rs = do
    root <- buildRootRules

    -- Proxy rule for the whole mingw toolchain on Windows.
    -- We 'need' configure  because that's when the inplace/mingw
    -- folder gets filled with the toolchain. This "proxy" rule
    -- is listed as a runtime dependency for stage >= 1 GHCs.
    root -/- mingwStamp %> \stampPath -> do
        top <- topDirectory
        need [ top -/- "configure" ]
        copyDirectory (top -/- "inplace" -/- "mingw") root
        writeFile' stampPath "OK"

    -- Rules for programs that are actually built by hadrian.
    forM_ allStages $ \stage ->
        [ root -/- stageString stage -/- "bin"     -/- "*"
        ] |%> \bin -> do
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
                 ++ tPackages
  fmap concat . forM allPackages $ \pkg -> do
    -- the iserv pkg results in three different programs at
    -- the moment, ghc-iserv (built the vanilla way),
    -- ghc-iserv-prof (built the profiling way),
    -- ghc-iserv-dyn (built the dynamic way), and
    -- ghc-iserv-prof-dyn (built the profiling+dynamic way).
    -- The testsuite requires all to be present, so we
    -- make sure that we cover these
    -- "prof-build-under-other-name" cases.
    -- iserv gets its names from Packages.hs:programName
    ctx <- programContext stage pkg -- TODO: see todo on programContext.
    let allCtxs = if pkg == iserv
        then [ vanillaContext stage pkg
             , Context stage pkg profiling Final
             , Context stage pkg dynamic Final
             , Context stage pkg profilingDynamic Final
             ]
        else [ ctx ]
    forM allCtxs $ \ctx -> do
      name <- programName ctx
      return (name <.> exe, ctx)

lookupProgramContext :: FilePath -> [(FilePath, Context)] -> Maybe Context
lookupProgramContext wholePath progs = lookup (takeFileName wholePath) progs

buildProgram :: FilePath -> Context -> [(Resource, Int)] -> Action ()
buildProgram bin ctx@(Context{..}) rs = do

  when (package == hsc2hs) $ do
    -- 'Hsc2hs' needs the @template-hsc.h@ file.
    template <- templateHscPath stage
    need [template]
  -- Custom dependencies: this should be modeled better in the
  -- Cabal file somehow.
  when (package == haddock) $ do
    -- Haddock has a resource folder
    need =<< haddockDeps stage

  -- Need library dependencies.
  -- Note pkgLibraryFile gets the path in the build dir e.g.
  --    _build/stage1/libraries/haskeline/build/libHShaskeline-0.7.5.0-ghc8.9.0.20190430.so
  -- but when building the program, we link against the *ghc-pkg registered* library e.g.
  --    _build/stage1/lib/x86_64-linux-ghc-8.9.0.20190430/libHShaskeline-0.7.5.0-ghc8.9.0.20190430.so
  -- so we use pkgRegisteredLibraryFile instead.
  registerPackages =<< contextDependencies ctx

  buildBinary rs bin ctx

buildBinary :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinary rs bin context@Context {..} = do
    needLibrary =<< contextDependencies context
    when (stage > stage0InTree) $ do
        ways <- interpretInContext context (getLibraryWays <> getRtsWays)
        needLibrary [ (rtsContext stage) { way = w } | w <- Set.toList ways ]
    asmSrcs <- interpretInContext context (getContextData asmSrcs)
    asmObjs <- mapM (objectPath context) asmSrcs
    cSrcs   <- interpretInContext context (getContextData cSrcs)
    cxxSrcs <- interpretInContext context (getContextData cxxSrcs)
    jsSrcs  <- interpretInContext context (getContextData jsSrcs)
    cObjs   <- mapM (objectPath context) cSrcs
    cxxObjs <- mapM (objectPath context) cxxSrcs
    jsObjs  <- mapM (objectPath context) jsSrcs
    hsObjs  <- hsObjects context
    let binDeps = asmObjs ++ cObjs ++ cxxObjs ++ jsObjs ++ hsObjs
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- pkgSynopsis package
    putSuccess $ renderProgram
        (quote (pkgName package) ++ " (" ++ show stage ++ ").") bin synopsis
