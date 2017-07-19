module Rules (topLevelTargets, packageTargets, buildRules) where

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
import Oracles.Dependencies
import Settings
import Settings.Path

allStages :: [Stage]
allStages = [minBound ..]

-- | This rule 'need' all top-level build targets
-- or Stage1Only targets
topLevelTargets :: Rules ()
topLevelTargets = action $ do
    need $ Rules.Generate.inplaceLibCopyTargets
    let libraryPackages = filter isLibrary (knownPackages \\ [rts, libffi])
    need =<< if stage1Only
             then do
                 libs <- concatForM [Stage0, Stage1] $ \stage ->
                     concatForM libraryPackages $ packageTargets stage
                 prgs <- concatForM programsStage1Only $ packageTargets Stage0
                 return $ libs ++ prgs
             else
                 concatForM allStages $ \stage ->
                     concatForM (knownPackages \\ [rts, libffi]) $ packageTargets stage

-- | Return the list of targets associated with a given 'Stage' and 'Package'.
packageTargets :: Stage -> Package -> Action [FilePath]
packageTargets stage pkg = do
    let context = vanillaContext stage pkg
    activePackages <- interpretInContext context getPackages
    if pkg `notElem` activePackages
    then return [] -- Skip inactive packages.
    else if isLibrary pkg
        then do -- Collect all targets of a library package.
            ways <- interpretInContext context getLibraryWays
            libs <- mapM (pkgLibraryFile . Context stage pkg) ways
            docs <- interpretInContext context $ buildHaddock flavour
            more <- libraryTargets context
            return $ [ pkgSetupConfigFile context | nonCabalContext context ]
                  ++ [ pkgHaddockFile     context | docs && stage == Stage1 ]
                  ++ libs ++ more
        else -- The only target of a program package is the executable.
            maybeToList <$> programPath (programContext stage pkg)

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
