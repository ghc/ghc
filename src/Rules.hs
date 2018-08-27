module Rules (buildRules, oracleRules, packageTargets, topLevelTargets) where

import qualified Hadrian.Oracles.ArgsHash
import qualified Hadrian.Oracles.DirectoryContents
import qualified Hadrian.Oracles.Path
import qualified Hadrian.Oracles.TextFile.Rules

import Expression
import GHC
import qualified Oracles.ModuleFiles
import qualified Rules.BinaryDist
import qualified Rules.Compile
import qualified Rules.Configure
import qualified Rules.Dependencies
import qualified Rules.Documentation
import qualified Rules.Generate
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Library
import qualified Rules.Program
import qualified Rules.Register
import Settings
import Target
import UserSettings
import Utilities

allStages :: [Stage]
allStages = [minBound .. maxBound]

-- | This rule calls 'need' on all top-level build targets that Hadrian builds
-- by default, respecting the 'stage1Only' flag.
topLevelTargets :: Rules ()
topLevelTargets = action $ do
    verbosity <- getVerbosity
    when (verbosity >= Loud) $ do
        (libraries, programs) <- partition isLibrary <$> stagePackages Stage1
        libNames <- mapM (name Stage1) libraries
        pgmNames <- mapM (name Stage1) programs
        putNormal . unlines $
            [ "| Building Stage1 libraries: " ++ intercalate ", " libNames
            , "| Building Stage1 programs : " ++ intercalate ", " pgmNames ]
    let buildStages = [Stage0, Stage1] ++ [Stage2 | not stage1Only]
    targets <- concatForM buildStages $ \stage -> do
        packages <- stagePackages stage
        mapM (path stage) packages
    need targets
  where
    -- either the package database config file for libraries or
    -- the programPath for programs. However this still does
    -- not support multiple targets, where a cabal package has
    -- a library /and/ a program.
    path :: Stage -> Package -> Action FilePath
    path stage pkg | isLibrary pkg = pkgConfFile (vanillaContext stage pkg)
                   | otherwise     = programPath =<< programContext stage pkg
    name :: Stage -> Package -> Action String
    name stage pkg | isLibrary pkg = return (pkgName pkg)
                   | otherwise     = programName (vanillaContext stage pkg)

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
            libs  <- mapM (pkgLibraryFile . Context stage pkg) ways
            more  <- libraryTargets includeGhciLib context
            setup <- pkgSetupConfigFile context
            return $ [setup] ++ libs ++ more
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
    let readPackageDb  = [(packageDb, 1)]
        writePackageDb = [(packageDb, maxConcurrentReaders)]

    let contexts        = liftM3 Context        allStages knownPackages allWays
        vanillaContexts = liftM2 vanillaContext allStages knownPackages

    -- TODO: we might want to look into converting more and more
    --       rules to the style introduced in Rules.Library in
    --       https://github.com/snowleopard/hadrian/pull/571,
    --       where "catch-all" rules are used to "catch" the need
    --       for library files, and we then use parsec parsers to
    --       extract all sorts of information needed to build them, like
    --       the package, the stage, the way, etc.

    forM_ contexts (Rules.Compile.compilePackage readPackageDb)

    Rules.Program.buildProgram readPackageDb

    forM_ [Stage0 .. ] $ \stage ->
        -- we create a dummy context, that has the correct state, but contains
        -- @dummyPackage@ as a... dummy package. The package isn't accessed but the record
        -- need to be set properly. @undefined@ is not an option as it ends up
        -- being forced.
        Rules.Register.registerPackage writePackageDb (Context stage dummyPackage vanilla)

    forM_ vanillaContexts $ mconcat
        [ Rules.Register.configurePackage
        , Rules.Dependencies.buildPackageDependencies readPackageDb
        , Rules.Documentation.buildPackageDocumentation
        , Rules.Generate.generatePackageCode ]

buildRules :: Rules ()
buildRules = do
    Rules.BinaryDist.bindistRules
    Rules.Configure.configureRules
    Rules.Generate.copyRules
    Rules.Generate.generateRules
    Rules.Gmp.gmpRules
    Rules.Libffi.libffiRules
    Rules.Library.libraryRules
    packageRules

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Hadrian.Oracles.DirectoryContents.directoryContentsOracle
    Hadrian.Oracles.Path.pathOracle
    Hadrian.Oracles.TextFile.Rules.textFileOracle
    Oracles.ModuleFiles.moduleFilesOracle
