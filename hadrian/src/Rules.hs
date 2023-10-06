module Rules (buildRules, oracleRules, packageTargets, topLevelTargets
             , toolArgsTarget ) where

import qualified Data.Set as Set

import qualified Hadrian.Oracles.ArgsHash
import qualified Hadrian.Oracles.Cabal.Rules
import qualified Hadrian.Oracles.DirectoryContents
import qualified Hadrian.Oracles.Path
import qualified Hadrian.Oracles.TextFile
import qualified Hadrian.Haskell.Hash

import Expression
import qualified Oracles.Flavour
import qualified Oracles.ModuleFiles
import Packages
import qualified Rules.BinaryDist
import qualified Rules.CabalReinstall
import qualified Rules.Compile
import qualified Rules.Dependencies
import qualified Rules.Documentation
import qualified Rules.Generate
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Library
import qualified Rules.Program
import qualified Rules.Register
import qualified Rules.Rts
import qualified Rules.SimpleTargets
import Rules.ToolArgs
import Settings
import Settings.Program (programContext)
import Target
import UserSettings

-- | This rule defines what the default build configuration is when no targets
-- are selected.
topLevelTargets :: Rules ()
topLevelTargets = action $ do
    let targets = ["binary-dist-dir"]

    -- Why we need wrappers: https://gitlab.haskell.org/ghc/ghc/issues/16534.
    root <- buildRoot
    let wrappers = [ root -/- ("ghc-" ++ stageString s) | s <- [Stage1, Stage2, Stage3]
                                                        , s < finalStage ]
    need (targets ++ wrappers)

-- TODO: Get rid of the @includeGhciLib@ hack.
-- | Return the list of targets associated with a given 'Stage' and 'Package'.
-- By setting the Boolean parameter to False it is possible to exclude the GHCi
-- library from the targets, and avoid configuring the package to determine
-- whether GHCi library needs to be built for it. We typically want to set
-- this parameter to True, however it is important to set it to False when
-- computing 'topLevelTargets', as otherwise the whole build gets sequentialised
-- because packages are configured in the order respecting their dependencies.
packageTargets :: Bool -> Stage -> Package -> Action [FilePath]
packageTargets includeGhciLib stage pkg = do
    let context = vanillaContext stage pkg
    activePackages <- stagePackages stage
    if pkg `notElem` activePackages
    then return [] -- Skip inactive packages.
    else if isLibrary pkg
        then do -- Collect all targets of a library package.
            let pkgWays = if pkg == rts then getRtsWays else getLibraryWays
            ways  <- interpretInContext context pkgWays
            libs  <- mapM (\w -> pkgLibraryFile (Context stage pkg w (error "unused"))) (Set.toList ways)
            more  <- Rules.Library.libraryTargets includeGhciLib context
            setupConfig <- pkgSetupConfigFile context
            return $ [setupConfig] ++ libs ++ more
        else do -- The only target of a program package is the executable.
            prgContext <- programContext stage pkg
            prgPath    <- programPath prgContext
            return [prgPath]

packageRules :: Rules ()
packageRules = do
    -- We cannot register multiple GHC packages in parallel. Also we cannot run
    -- GHC when the package database is being mutated by "ghc-pkg". This is a
    -- classic concurrent read exclusive write (CREW) conflict.
    let maxConcurrentReaders = 1000
    packageDb <- newResource "package-db" maxConcurrentReaders
    let readPackageDb  = [(packageDb, 1)]                    -- this is correct: take 1 slot to read
        writePackageDb = [(packageDb, maxConcurrentReaders)] -- and all the slots to write

    Rules.Compile.compilePackage readPackageDb
    Rules.Dependencies.buildPackageDependencies readPackageDb
    Rules.Documentation.buildPackageDocumentation
    Rules.Program.buildProgramRules readPackageDb
    Rules.Register.configurePackageRules

    forM_ [Inplace, Final] $ \iplace -> forM_ allStages $ \stage -> (Rules.Register.registerPackageRules writePackageDb stage iplace)

    -- TODO: Can we get rid of this enumeration of contexts? Since we iterate
    --       over it to generate all 4 types of rules below, all the time, we
    --       might want to see whether the parse-and-extract approach of
    --       Rules.Compile and Rules.Library could save us some time there.
    let vanillaContexts = liftM2 vanillaContext allStages knownPackages

    forM_ vanillaContexts Rules.Generate.generatePackageCode
    Rules.SimpleTargets.simplePackageTargets
    Rules.SimpleTargets.completionRule

buildRules :: Rules ()
buildRules = do
    Rules.BinaryDist.bindistRules
    Rules.Generate.copyRules
    Rules.Generate.generateRules
    Rules.Generate.templateRules
    Rules.Gmp.gmpRules
    Rules.Libffi.libffiRules
    Rules.Library.libraryRules
    Rules.Rts.rtsRules
    packageRules
    Rules.CabalReinstall.cabalBuildRules

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Hadrian.Oracles.Cabal.Rules.cabalOracle
    Hadrian.Haskell.Hash.pkgHashOracle
    Hadrian.Oracles.DirectoryContents.directoryContentsOracle
    Hadrian.Oracles.Path.pathOracle
    Hadrian.Oracles.TextFile.textFileOracle
    Oracles.Flavour.oracles
    Oracles.ModuleFiles.moduleFilesOracle
