module Rules (topLevelTargets, buildPackage, buildRules) where

import Base
import Context
import Expression
import Flavour
import GHC
import qualified Rules.Compile
import qualified Rules.Data
import qualified Rules.Dependencies
import qualified Rules.Documentation
import qualified Rules.Generate
import qualified Rules.Cabal
import qualified Rules.Configure
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Library
import qualified Rules.Perl
import qualified Rules.Program
import qualified Rules.Register
import Oracles.Dependencies (needContext)
import Util (needBuilder)
import Settings
import Settings.Path

allStages :: [Stage]
allStages = [minBound ..]

-- | This rule 'need' all top-level build targets
-- or Stage1Only targets
topLevelTargets :: Rules ()
topLevelTargets = action $ do
    need $ Rules.Generate.inplaceLibCopyTargets

    if stage1Only
        then do
             forAllPkgs $ \stg pkg ->
                 when (isLibrary pkg) $
                     buildPackage stg pkg
             forM_ programsStage1Only $ buildPackage Stage0
        else
             forAllPkgs buildPackage
  where
    forAllPkgs f =
      forM_ allStages $ \stage ->
          forM_ (knownPackages \\ [rts, libffi]) $ \pkg -> f stage pkg

buildPackage :: Stage -> Package -> Action ()
buildPackage stage pkg = do
    let context = vanillaContext stage pkg
    activePackages <- interpretInContext context getPackages
    when (pkg `elem` activePackages) $
        if isLibrary pkg
        then do -- build a library
            when (nonCabalContext context) $
                need [pkgSetupConfigFile context]
            ways <- interpretInContext context getLibraryWays
            libs <- mapM (pkgLibraryFile . Context stage pkg) ways
            docs <- interpretInContext context $ buildHaddock flavour
            needContext [context]
            need $ libs ++ [ pkgHaddockFile context | docs && stage == Stage1 ]
        else -- otherwise build a program
            need =<< maybeToList <$> programPath (programContext stage pkg)

packageRules :: Rules ()
packageRules = do
    -- We cannot register multiple GHC packages in parallel. Also we cannot run
    -- GHC when the package database is being mutated by "ghc-pkg". This is a
    -- classic concurrent read exclusive write (CREW) conflict.
    let maxConcurrentReaders = 1000
    packageDb <- newResource "package-db" maxConcurrentReaders
    let readPackageDb  = [(packageDb, 1)]
        writePackageDb = [(packageDb, maxConcurrentReaders)]

    let contexts        = liftM3 Context        allStages knownPackages allWays
        vanillaContexts = liftM2 vanillaContext allStages knownPackages
        programContexts = liftM2 programContext allStages knownPackages

    forM_ contexts $ mconcat
        [ Rules.Compile.compilePackage readPackageDb
        , Rules.Library.buildPackageLibrary ]

    let dynamicContexts = liftM3 Context [Stage1 ..] knownPackages [dynamic]

    forM_ dynamicContexts Rules.Library.buildDynamicLib

    forM_ programContexts $ Rules.Program.buildProgram readPackageDb

    forM_ vanillaContexts $ mconcat
        [ Rules.Data.buildPackageData
        , Rules.Dependencies.buildPackageDependencies readPackageDb
        , Rules.Documentation.buildPackageDocumentation
        , Rules.Library.buildPackageGhciLibrary
        , Rules.Generate.generatePackageCode
        , Rules.Register.registerPackage writePackageDb ]

buildRules :: Rules ()
buildRules = do
    Rules.Cabal.cabalRules
    Rules.Configure.configureRules
    Rules.Generate.copyRules
    Rules.Generate.generateRules
    Rules.Gmp.gmpRules
    Rules.Libffi.libffiRules
    packageRules
    Rules.Perl.perlScriptRules

programsStage1Only :: [Package]
programsStage1Only =
  [ deriveConstants, genprimopcode, hp2ps, runGhc
  , ghcCabal, hpc, dllSplit, ghcPkg, hsc2hs
  , genapply, ghc ]
