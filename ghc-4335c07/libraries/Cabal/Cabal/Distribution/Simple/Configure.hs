{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This deals with the /configure/ phase. It provides the 'configure' action
-- which is given the package description and configure flags. It then tries
-- to: configure the compiler; resolves any conditionals in the package
-- description; resolve the package dependencies; check if all the extensions
-- used by this package are supported by the compiler; check that all the build
-- tools are available (including version checks if appropriate); checks for
-- any required @pkg-config@ packages (updating the 'BuildInfo' with the
-- results)
--
-- Then based on all this it saves the info in the 'LocalBuildInfo' and writes
-- it out to the @dist\/setup-config@ file. It also displays various details to
-- the user, the amount of information displayed depending on the verbosity
-- level.

module Distribution.Simple.Configure (configure,
                                      writePersistBuildConfig,
                                      getConfigStateFile,
                                      getPersistBuildConfig,
                                      checkPersistBuildConfigOutdated,
                                      tryGetPersistBuildConfig,
                                      maybeGetPersistBuildConfig,
                                      findDistPref, findDistPrefOrDefault,
                                      getInternalPackages,
                                      computeComponentId,
                                      computeCompatPackageKey,
                                      computeCompatPackageName,
                                      localBuildInfoFile,
                                      getInstalledPackages,
                                      getInstalledPackagesMonitorFiles,
                                      getPackageDBContents,
                                      configCompiler, configCompilerAux,
                                      configCompilerEx, configCompilerAuxEx,
                                      computeEffectiveProfiling,
                                      ccLdOptionsBuildInfo,
                                      checkForeignDeps,
                                      interpretPackageDbFlags,
                                      ConfigStateFileError(..),
                                      tryGetConfigStateFile,
                                      platformDefines,
                                     )
    where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Compiler
import Distribution.Types.IncludeRenaming
import Distribution.Utils.NubList
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.PreProcess
import Distribution.Package
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Types.PackageDescription as PD
import Distribution.PackageDescription.PrettyPrint
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Check hiding (doesFileExist)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.Program
import Distribution.Simple.Setup as Setup
import Distribution.Simple.BuildTarget
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PkgconfigDependency
import Distribution.Types.MungedPackageName
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.ForeignLibOption
import Distribution.Types.Mixin
import Distribution.Types.UnqualComponentName
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Version
import Distribution.Verbosity
import qualified Distribution.Compat.Graph as Graph
import Distribution.Compat.Stack
import Distribution.Backpack.Configure
import Distribution.Backpack.DescribeUnitId
import Distribution.Backpack.PreExistingComponent
import Distribution.Backpack.ConfiguredComponent (newPackageDepsBehaviour)
import Distribution.Backpack.Id
import Distribution.Utils.LogProgress

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite

import Control.Exception
    ( ErrorCall, Exception, evaluate, throw, throwIO, try )
import Distribution.Compat.Binary ( decodeOrFailIO, encode )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.List
    ( (\\), partition, inits, stripPrefix )
import Data.Either
    ( partitionEithers )
import qualified Data.Map as Map
import System.Directory
    ( doesFileExist, createDirectoryIfMissing, getTemporaryDirectory )
import System.FilePath
    ( (</>), isAbsolute )
import qualified System.Info
    ( compilerName, compilerVersion )
import System.IO
    ( hPutStrLn, hClose )
import Distribution.Text
    ( Text(disp), defaultStyle, display, simpleParse )
import Text.PrettyPrint
    ( Doc, (<+>), ($+$), char, comma, hsep, nest
    , punctuate, quotes, render, renderStyle, sep, text )
import Distribution.Compat.Environment ( lookupEnv )
import Distribution.Compat.Exception ( catchExit, catchIO )

type UseExternalInternalDeps = Bool

-- | The errors that can be thrown when reading the @setup-config@ file.
data ConfigStateFileError
    = ConfigStateFileNoHeader -- ^ No header found.
    | ConfigStateFileBadHeader -- ^ Incorrect header.
    | ConfigStateFileNoParse -- ^ Cannot parse file contents.
    | ConfigStateFileMissing -- ^ No file!
    | ConfigStateFileBadVersion PackageIdentifier PackageIdentifier
      (Either ConfigStateFileError LocalBuildInfo) -- ^ Mismatched version.
  deriving (Typeable)

-- | Format a 'ConfigStateFileError' as a user-facing error message.
dispConfigStateFileError :: ConfigStateFileError -> Doc
dispConfigStateFileError ConfigStateFileNoHeader =
    text "Saved package config file header is missing."
    <+> text "Re-run the 'configure' command."
dispConfigStateFileError ConfigStateFileBadHeader =
    text "Saved package config file header is corrupt."
    <+> text "Re-run the 'configure' command."
dispConfigStateFileError ConfigStateFileNoParse =
    text "Saved package config file is corrupt."
    <+> text "Re-run the 'configure' command."
dispConfigStateFileError ConfigStateFileMissing =
    text "Run the 'configure' command first."
dispConfigStateFileError (ConfigStateFileBadVersion oldCabal oldCompiler _) =
    text "Saved package config file is outdated:"
    $+$ badCabal $+$ badCompiler
    $+$ text "Re-run the 'configure' command."
    where
      badCabal =
          text "• the Cabal version changed from"
          <+> disp oldCabal <+> "to" <+> disp currentCabalId
      badCompiler
        | oldCompiler == currentCompilerId = mempty
        | otherwise =
            text "• the compiler changed from"
            <+> disp oldCompiler <+> "to" <+> disp currentCompilerId

instance Show ConfigStateFileError where
    show = renderStyle defaultStyle . dispConfigStateFileError

instance Exception ConfigStateFileError

-- | Read the 'localBuildInfoFile'.  Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getConfigStateFile :: FilePath -- ^ The file path of the @setup-config@ file.
                   -> IO LocalBuildInfo
getConfigStateFile filename = do
    exists <- doesFileExist filename
    unless exists $ throwIO ConfigStateFileMissing
    -- Read the config file into a strict ByteString to avoid problems with
    -- lazy I/O, then convert to lazy because the binary package needs that.
    contents <- BS.readFile filename
    let (header, body) = BLC8.span (/='\n') (BLC8.fromChunks [contents])

    headerParseResult <- try $ evaluate $ parseHeader header
    let (cabalId, compId) =
            case headerParseResult of
              Left (_ :: ErrorCall) -> throw ConfigStateFileBadHeader
              Right x -> x

    let getStoredValue = do
          result <- decodeOrFailIO (BLC8.tail body)
          case result of
            Left _ -> throw ConfigStateFileNoParse
            Right x -> return x
        deferErrorIfBadVersion act
          | cabalId /= currentCabalId = do
              eResult <- try act
              throw $ ConfigStateFileBadVersion cabalId compId eResult
          | otherwise = act
    deferErrorIfBadVersion getStoredValue
  where
    _ = callStack -- TODO: attach call stack to exception

-- | Read the 'localBuildInfoFile', returning either an error or the local build
-- info.
tryGetConfigStateFile :: FilePath -- ^ The file path of the @setup-config@ file.
                      -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetConfigStateFile = try . getConfigStateFile

-- | Try to read the 'localBuildInfoFile'.
tryGetPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                         -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetPersistBuildConfig = try . getPersistBuildConfig

-- | Read the 'localBuildInfoFile'. Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                      -> IO LocalBuildInfo
getPersistBuildConfig = getConfigStateFile . localBuildInfoFile

-- | Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                           -> IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig =
    liftM (either (const Nothing) Just) . tryGetPersistBuildConfig

-- | After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                        -> LocalBuildInfo -- ^ The 'LocalBuildInfo' to write.
                        -> NoCallStackIO ()
writePersistBuildConfig distPref lbi = do
    createDirectoryIfMissing False distPref
    writeFileAtomic (localBuildInfoFile distPref) $
      BLC8.unlines [showHeader pkgId, encode lbi]
  where
    pkgId = localPackage lbi

-- | Identifier of the current Cabal package.
currentCabalId :: PackageIdentifier
currentCabalId = PackageIdentifier (mkPackageName "Cabal") cabalVersion

-- | Identifier of the current compiler package.
currentCompilerId :: PackageIdentifier
currentCompilerId = PackageIdentifier (mkPackageName System.Info.compilerName)
                                      (mkVersion' System.Info.compilerVersion)

-- | Parse the @setup-config@ file header, returning the package identifiers
-- for Cabal and the compiler.
parseHeader :: ByteString -- ^ The file contents.
            -> (PackageIdentifier, PackageIdentifier)
parseHeader header = case BLC8.words header of
  ["Saved", "package", "config", "for", pkgId, "written", "by", cabalId,
   "using", compId] ->
      fromMaybe (throw ConfigStateFileBadHeader) $ do
          _ <- simpleParse (BLC8.unpack pkgId) :: Maybe PackageIdentifier
          cabalId' <- simpleParse (BLC8.unpack cabalId)
          compId' <- simpleParse (BLC8.unpack compId)
          return (cabalId', compId')
  _ -> throw ConfigStateFileNoHeader

-- | Generate the @setup-config@ file header.
showHeader :: PackageIdentifier -- ^ The processed package.
            -> ByteString
showHeader pkgId = BLC8.unwords
    [ "Saved", "package", "config", "for"
    , BLC8.pack $ display pkgId
    , "written", "by"
    , BLC8.pack $ display currentCabalId
    , "using"
    , BLC8.pack $ display currentCompilerId
    ]

-- | Check that localBuildInfoFile is up-to-date with respect to the
-- .cabal file.
checkPersistBuildConfigOutdated :: FilePath -> FilePath -> NoCallStackIO Bool
checkPersistBuildConfigOutdated distPref pkg_descr_file =
  pkg_descr_file `moreRecentFile` localBuildInfoFile distPref

-- | Get the path of @dist\/setup-config@.
localBuildInfoFile :: FilePath -- ^ The @dist@ directory path.
                    -> FilePath
localBuildInfoFile distPref = distPref </> "setup-config"

-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken
-- from (in order of highest to lowest preference) the override prefix, the
-- \"CABAL_BUILDDIR\" environment variable, or the default prefix.
findDistPref :: FilePath  -- ^ default \"dist\" prefix
             -> Setup.Flag FilePath  -- ^ override \"dist\" prefix
             -> NoCallStackIO FilePath
findDistPref defDistPref overrideDistPref = do
    envDistPref <- liftM parseEnvDistPref (lookupEnv "CABAL_BUILDDIR")
    return $ fromFlagOrDefault defDistPref (mappend envDistPref overrideDistPref)
  where
    parseEnvDistPref env =
      case env of
        Just distPref | not (null distPref) -> toFlag distPref
        _ -> NoFlag

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken
-- from (in order of highest to lowest preference) the override prefix, the
-- \"CABAL_BUILDDIR\" environment variable, or 'defaultDistPref' is used. Call
-- this function to resolve a @*DistPref@ flag whenever it is not known to be
-- set. (The @*DistPref@ flags are always set to a definite value before
-- invoking 'UserHooks'.)
findDistPrefOrDefault :: Setup.Flag FilePath  -- ^ override \"dist\" prefix
                      -> NoCallStackIO FilePath
findDistPrefOrDefault = findDistPref defaultDistPref

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: (GenericPackageDescription, HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0, pbi) cfg = do
    -- Determine the component we are configuring, if a user specified
    -- one on the command line.  We use a fake, flattened version of
    -- the package since at this point, we're not really sure what
    -- components we *can* configure.  @Nothing@ means that we should
    -- configure everything (the old behavior).
    (mb_cname :: Maybe ComponentName) <- do
        let flat_pkg_descr = flattenPackageDescription pkg_descr0
        targets <- readBuildTargets verbosity flat_pkg_descr (configArgs cfg)
        -- TODO: bleat if you use the module/file syntax
        let targets' = [ cname | BuildTargetComponent cname <- targets ]
        case targets' of
            _ | null (configArgs cfg) -> return Nothing
            [cname] -> return (Just cname)
            [] -> die' verbosity "No valid component targets found"
            _ -> die' verbosity "Can only configure either single component or all of them"

    let use_external_internal_deps = isJust mb_cname
    case mb_cname of
        Nothing -> setupMessage verbosity "Configuring" (packageId pkg_descr0)
        Just cname -> setupMessage' verbosity "Configuring" (packageId pkg_descr0)
                        cname (Just (configInstantiateWith cfg))

    -- configCID is only valid for per-component configure
    when (isJust (flagToMaybe (configCID cfg)) && isNothing mb_cname) $
        die' verbosity "--cid is only supported for per-component configure"

    checkDeprecatedFlags verbosity cfg
    checkExactConfiguration verbosity pkg_descr0 cfg

    -- Where to build the package
    let buildDir :: FilePath -- e.g. dist/build
        -- fromFlag OK due to Distribution.Simple calling
        -- findDistPrefOrDefault to fill it in
        buildDir = fromFlag (configDistPref cfg) </> "build"
    createDirectoryIfMissingVerbose (lessVerbose verbosity) True buildDir

    -- What package database(s) to use
    let packageDbs :: PackageDBStack
        packageDbs
         = interpretPackageDbFlags
            (fromFlag (configUserInstall cfg))
            (configPackageDBs cfg)

    -- comp:            the compiler we're building with
    -- compPlatform:    the platform we're building for
    -- programDb:  location and args of all programs we're
    --                  building with
    (comp         :: Compiler,
     compPlatform :: Platform,
     programDb    :: ProgramDb)
        <- configCompilerEx
            (flagToMaybe (configHcFlavor cfg))
            (flagToMaybe (configHcPath cfg))
            (flagToMaybe (configHcPkg cfg))
            (mkProgramDb cfg (configPrograms cfg))
            (lessVerbose verbosity)

    -- The InstalledPackageIndex of all installed packages
    installedPackageSet :: InstalledPackageIndex
        <- getInstalledPackages (lessVerbose verbosity) comp
                                  packageDbs programDb

    -- The set of package names which are "shadowed" by internal
    -- packages, and which component they map to
    let internalPackageSet :: Map PackageName (Maybe UnqualComponentName)
        internalPackageSet = getInternalPackages pkg_descr0

    -- Make a data structure describing what components are enabled.
    let enabled :: ComponentRequestedSpec
        enabled = case mb_cname of
                    Just cname -> OneComponentRequestedSpec cname
                    Nothing -> ComponentRequestedSpec
                                -- The flag name (@--enable-tests@) is a
                                -- little bit of a misnomer, because
                                -- just passing this flag won't
                                -- "enable", in our internal
                                -- nomenclature; it's just a request; a
                                -- @buildable: False@ might make it
                                -- not possible to enable.
                                { testsRequested = fromFlag (configTests cfg)
                                , benchmarksRequested =
                                  fromFlag (configBenchmarks cfg) }
    -- Some sanity checks related to enabling components.
    when (isJust mb_cname
          && (fromFlag (configTests cfg) || fromFlag (configBenchmarks cfg))) $
        die' verbosity $ "--enable-tests/--enable-benchmarks are incompatible with" ++
              " explicitly specifying a component to configure."

    -- allConstraints:  The set of all 'Dependency's we have.  Used ONLY
    --                  to 'configureFinalizedPackage'.
    -- requiredDepsMap: A map from 'PackageName' to the specifically
    --                  required 'InstalledPackageInfo', due to --dependency
    --
    -- NB: These constraints are to be applied to ALL components of
    -- a package.  Thus, it's not an error if allConstraints contains
    -- more constraints than is necessary for a component (another
    -- component might need it.)
    --
    -- NB: The fact that we bundle all the constraints together means
    -- that is not possible to configure a test-suite to use one
    -- version of a dependency, and the executable to use another.
    (allConstraints  :: [Dependency],
     requiredDepsMap :: Map PackageName InstalledPackageInfo)
        <- either (die' verbosity) return $
              combinedConstraints (configConstraints cfg)
                                  (configDependencies cfg)
                                  installedPackageSet

    -- pkg_descr:   The resolved package description, that does not contain any
    --              conditionals, because we have have an assignment for
    --              every flag, either picking them ourselves using a
    --              simple naive algorithm, or having them be passed to
    --              us by 'configConfigurationsFlags')
    -- flags:       The 'FlagAssignment' that the conditionals were
    --              resolved with.
    --
    -- NB: Why doesn't finalizing a package also tell us what the
    -- dependencies are (e.g. when we run the naive algorithm,
    -- we are checking if dependencies are satisfiable)?  The
    -- primary reason is that we may NOT have done any solving:
    -- if the flags are all chosen for us, this step is a simple
    -- matter of flattening according to that assignment.  It's
    -- cleaner to then configure the dependencies afterwards.
    (pkg_descr :: PackageDescription,
     flags     :: FlagAssignment)
        <- configureFinalizedPackage verbosity cfg enabled
                allConstraints
                (dependencySatisfiable
                    use_external_internal_deps
                    (fromFlagOrDefault False (configExactConfiguration cfg))
                    (packageName pkg_descr0)
                    installedPackageSet
                    internalPackageSet
                    requiredDepsMap)
                comp
                compPlatform
                pkg_descr0

    debug verbosity $ "Finalized package description:\n"
                  ++ showPackageDescription pkg_descr
    -- NB: showPackageDescription does not display the AWFUL HACK GLOBAL
    -- buildDepends, so we have to display it separately.  See #2066
    -- Some day, we should eliminate this, so that
    -- configureFinalizedPackage returns the set of overall dependencies
    -- separately.  Then 'configureDependencies' and
    -- 'Distribution.PackageDescription.Check' need to be adjusted
    -- accordingly.
    debug verbosity $ "Finalized build-depends: "
                  ++ intercalate ", " (map display (buildDepends pkg_descr))

    checkCompilerProblems verbosity comp pkg_descr enabled
    checkPackageProblems verbosity pkg_descr0
        (updatePackageDescription pbi pkg_descr)

    -- The list of 'InstalledPackageInfo' recording the selected
    -- dependencies on external packages.
    --
    -- Invariant: For any package name, there is at most one package
    -- in externalPackageDeps which has that name.
    --
    -- NB: The dependency selection is global over ALL components
    -- in the package (similar to how allConstraints and
    -- requiredDepsMap are global over all components).  In particular,
    -- if *any* component (post-flag resolution) has an unsatisfiable
    -- dependency, we will fail.  This can sometimes be undesirable
    -- for users, see #1786 (benchmark conflicts with executable),
    --
    -- In the presence of Backpack, these package dependencies are
    -- NOT complete: they only ever include the INDEFINITE
    -- dependencies.  After we apply an instantiation, we'll get
    -- definite references which constitute extra dependencies.
    -- (Why not have cabal-install pass these in explicitly?
    -- For one it's deterministic; for two, we need to associate
    -- them with renamings which would require a far more complicated
    -- input scheme than what we have today.)
    externalPkgDeps :: [PreExistingComponent]
        <- configureDependencies
                verbosity
                use_external_internal_deps
                internalPackageSet
                installedPackageSet
                requiredDepsMap
                pkg_descr

    -- Compute installation directory templates, based on user
    -- configuration.
    --
    -- TODO: Move this into a helper function.
    defaultDirs :: InstallDirTemplates
        <- defaultInstallDirs' use_external_internal_deps
                              (compilerFlavor comp)
                              (fromFlag (configUserInstall cfg))
                              (hasLibs pkg_descr)
    let installDirs :: InstallDirTemplates
        installDirs = combineInstallDirs fromFlagOrDefault
                        defaultDirs (configInstallDirs cfg)

    -- Check languages and extensions
    -- TODO: Move this into a helper function.
    let langlist = nub $ catMaybes $ map defaultLanguage
                   (enabledBuildInfos pkg_descr enabled)
    let langs = unsupportedLanguages comp langlist
    when (not (null langs)) $
      die' verbosity $ "The package " ++ display (packageId pkg_descr0)
         ++ " requires the following languages which are not "
         ++ "supported by " ++ display (compilerId comp) ++ ": "
         ++ intercalate ", " (map display langs)
    let extlist = nub $ concatMap allExtensions (enabledBuildInfos pkg_descr enabled)
    let exts = unsupportedExtensions comp extlist
    when (not (null exts)) $
      die' verbosity $ "The package " ++ display (packageId pkg_descr0)
         ++ " requires the following language extensions which are not "
         ++ "supported by " ++ display (compilerId comp) ++ ": "
         ++ intercalate ", " (map display exts)

    -- Check foreign library build requirements
    let flibs = [flib | CFLib flib <- enabledComponents pkg_descr enabled]
    let unsupportedFLibs = unsupportedForeignLibs comp compPlatform flibs
    when (not (null unsupportedFLibs)) $
      die' verbosity $ "Cannot build some foreign libraries: "
         ++ intercalate "," unsupportedFLibs

    -- Configure certain external build tools, see below for which ones.
    let requiredBuildTools = do
          bi <- enabledBuildInfos pkg_descr enabled
          -- First, we collect any tool dep that we know is external. This is,
          -- in practice:
          --
          -- 1. `build-tools` entries on the whitelist
          --
          -- 2. `build-tool-depends` that aren't from the current package.
          let externBuildToolDeps =
                [ LegacyExeDependency (unUnqualComponentName eName) versionRange
                | buildTool@(ExeDependency _ eName versionRange)
                  <- getAllToolDependencies pkg_descr bi
                , not $ isInternal pkg_descr buildTool ]
          -- Second, we collect any build-tools entry we don't know how to
          -- desugar. We'll never have any idea how to build them, so we just
          -- hope they are already on the PATH.
          let unknownBuildTools =
                [ buildTool
                | buildTool <- buildTools bi
                , Nothing == desugarBuildTool pkg_descr buildTool ]
          externBuildToolDeps ++ unknownBuildTools

    programDb' <-
          configureAllKnownPrograms (lessVerbose verbosity) programDb
      >>= configureRequiredPrograms verbosity requiredBuildTools

    (pkg_descr', programDb'') <-
      configurePkgconfigPackages verbosity pkg_descr programDb' enabled

    -- Compute internal component graph
    --
    -- The general idea is that we take a look at all the source level
    -- components (which may build-depends on each other) and form a graph.
    -- From there, we build a ComponentLocalBuildInfo for each of the
    -- components, which lets us actually build each component.
    -- internalPackageSet
    -- use_external_internal_deps
    (buildComponents :: [ComponentLocalBuildInfo],
     packageDependsIndex :: InstalledPackageIndex) <-
      runLogProgress verbosity $ configureComponentLocalBuildInfos
            verbosity
            use_external_internal_deps
            enabled
            (fromFlagOrDefault False (configDeterministic cfg))
            (configIPID cfg)
            (configCID cfg)
            pkg_descr
            externalPkgDeps
            (configConfigurationsFlags cfg)
            (configInstantiateWith cfg)
            installedPackageSet
            comp

    -- Decide if we're going to compile with split objects.
    split_objs :: Bool <-
       if not (fromFlag $ configSplitObjs cfg)
            then return False
            else case compilerFlavor comp of
                        GHC | compilerVersion comp >= mkVersion [6,5]
                          -> return True
                        GHCJS
                          -> return True
                        _ -> do warn verbosity
                                     ("this compiler does not support " ++
                                      "--enable-split-objs; ignoring")
                                return False

    let ghciLibByDefault =
          case compilerId comp of
            CompilerId GHC _ ->
              -- If ghc is non-dynamic, then ghci needs object files,
              -- so we build one by default.
              --
              -- Technically, archive files should be sufficient for ghci,
              -- but because of GHC bug #8942, it has never been safe to
              -- rely on them. By the time that bug was fixed, ghci had
              -- been changed to read shared libraries instead of archive
              -- files (see next code block).
              not (GHC.isDynamic comp)
            CompilerId GHCJS _ ->
              not (GHCJS.isDynamic comp)
            _ -> False

    let sharedLibsByDefault
          | fromFlag (configDynExe cfg) =
              -- build a shared library if dynamically-linked
              -- executables are requested
              True
          | otherwise = case compilerId comp of
            CompilerId GHC _ ->
              -- if ghc is dynamic, then ghci needs a shared
              -- library, so we build one by default.
              GHC.isDynamic comp
            CompilerId GHCJS _ ->
              GHCJS.isDynamic comp
            _ -> False
        withSharedLib_ =
            -- build shared libraries if required by GHC or by the
            -- executable linking mode, but allow the user to force
            -- building only static library archives with
            -- --disable-shared.
            fromFlagOrDefault sharedLibsByDefault $ configSharedLib cfg

        withStaticLib_ =
            -- build a static library (all dependent libraries rolled
            -- into a huge .a archive) via GHCs -staticlib flag.
            fromFlagOrDefault False $ configStaticLib cfg

        withDynExe_ = fromFlag $ configDynExe cfg
    when (withDynExe_ && not withSharedLib_) $ warn verbosity $
           "Executables will use dynamic linking, but a shared library "
        ++ "is not being built. Linking will fail if any executables "
        ++ "depend on the library."

    setProfLBI <- configureProfiling verbosity cfg comp

    setCoverageLBI <- configureCoverage verbosity cfg comp

    let reloc = fromFlagOrDefault False $ configRelocatable cfg

    let buildComponentsMap =
            foldl' (\m clbi -> Map.insertWith (++)
                               (componentLocalName clbi) [clbi] m)
                   Map.empty buildComponents

    let lbi = (setCoverageLBI . setProfLBI)
              LocalBuildInfo {
                configFlags         = cfg,
                flagAssignment      = flags,
                componentEnabledSpec = enabled,
                extraConfigArgs     = [],  -- Currently configure does not
                                           -- take extra args, but if it
                                           -- did they would go here.
                installDirTemplates = installDirs,
                compiler            = comp,
                hostPlatform        = compPlatform,
                buildDir            = buildDir,
                componentGraph      = Graph.fromDistinctList buildComponents,
                componentNameMap    = buildComponentsMap,
                installedPkgs       = packageDependsIndex,
                pkgDescrFile        = Nothing,
                localPkgDescr       = pkg_descr',
                withPrograms        = programDb'',
                withVanillaLib      = fromFlag $ configVanillaLib cfg,
                withSharedLib       = withSharedLib_,
                withStaticLib       = withStaticLib_,
                withDynExe          = withDynExe_,
                withProfLib         = False,
                withProfLibDetail   = ProfDetailNone,
                withProfExe         = False,
                withProfExeDetail   = ProfDetailNone,
                withOptimization    = fromFlag $ configOptimization cfg,
                withDebugInfo       = fromFlag $ configDebugInfo cfg,
                withGHCiLib         = fromFlagOrDefault ghciLibByDefault $
                                      configGHCiLib cfg,
                splitObjs           = split_objs,
                stripExes           = fromFlag $ configStripExes cfg,
                stripLibs           = fromFlag $ configStripLibs cfg,
                exeCoverage         = False,
                libCoverage         = False,
                withPackageDB       = packageDbs,
                progPrefix          = fromFlag $ configProgPrefix cfg,
                progSuffix          = fromFlag $ configProgSuffix cfg,
                relocatable         = reloc
              }

    when reloc (checkRelocatable verbosity pkg_descr lbi)

    -- TODO: This is not entirely correct, because the dirs may vary
    -- across libraries/executables
    let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
        relative = prefixRelativeInstallDirs (packageId pkg_descr) lbi

    unless (isAbsolute (prefix dirs)) $ die' verbosity $
        "expected an absolute directory name for --prefix: " ++ prefix dirs

    info verbosity $ "Using " ++ display currentCabalId
                  ++ " compiled by " ++ display currentCompilerId
    info verbosity $ "Using compiler: " ++ showCompilerId comp
    info verbosity $ "Using install prefix: " ++ prefix dirs

    let dirinfo name dir isPrefixRelative =
          info verbosity $ name ++ " installed in: " ++ dir ++ relNote
          where relNote = case buildOS of
                  Windows | not (hasLibs pkg_descr)
                         && isNothing isPrefixRelative
                         -> "  (fixed location)"
                  _      -> ""

    dirinfo "Executables"      (bindir dirs)     (bindir relative)
    dirinfo "Libraries"        (libdir dirs)     (libdir relative)
    dirinfo "Dynamic Libraries" (dynlibdir dirs) (dynlibdir relative)
    dirinfo "Private executables" (libexecdir dirs) (libexecdir relative)
    dirinfo "Data files"       (datadir dirs)    (datadir relative)
    dirinfo "Documentation"    (docdir dirs)     (docdir relative)
    dirinfo "Configuration files" (sysconfdir dirs) (sysconfdir relative)

    sequence_ [ reportProgram verbosity prog configuredProg
              | (prog, configuredProg) <- knownPrograms programDb'' ]

    return lbi

    where
      verbosity = fromFlag (configVerbosity cfg)

mkProgramDb :: ConfigFlags -> ProgramDb -> ProgramDb
mkProgramDb cfg initialProgramDb = programDb
  where
    programDb  = userSpecifyArgss (configProgramArgs cfg)
                 . userSpecifyPaths (configProgramPaths cfg)
                 . setProgramSearchPath searchpath
                 $ initialProgramDb
    searchpath = getProgramSearchPath initialProgramDb
                 ++ map ProgramSearchPathDir
                 (fromNubList $ configProgramPathExtra cfg)

-- -----------------------------------------------------------------------------
-- Helper functions for configure

-- | Check if the user used any deprecated flags.
checkDeprecatedFlags :: Verbosity -> ConfigFlags -> IO ()
checkDeprecatedFlags verbosity cfg = do
    unless (configProfExe cfg == NoFlag) $ do
      let enable | fromFlag (configProfExe cfg) = "enable"
                 | otherwise = "disable"
      warn verbosity
        ("The flag --" ++ enable ++ "-executable-profiling is deprecated. "
         ++ "Please use --" ++ enable ++ "-profiling instead.")

    unless (configLibCoverage cfg == NoFlag) $ do
      let enable | fromFlag (configLibCoverage cfg) = "enable"
                 | otherwise = "disable"
      warn verbosity
        ("The flag --" ++ enable ++ "-library-coverage is deprecated. "
         ++ "Please use --" ++ enable ++ "-coverage instead.")

-- | Sanity check: if '--exact-configuration' was given, ensure that the
-- complete flag assignment was specified on the command line.
checkExactConfiguration :: Verbosity -> GenericPackageDescription -> ConfigFlags -> IO ()
checkExactConfiguration verbosity pkg_descr0 cfg =
    when (fromFlagOrDefault False (configExactConfiguration cfg)) $ do
      let cmdlineFlags = map fst (unFlagAssignment (configConfigurationsFlags cfg))
          allFlags     = map flagName . genPackageFlags $ pkg_descr0
          diffFlags    = allFlags \\ cmdlineFlags
      when (not . null $ diffFlags) $
        die' verbosity $ "'--exact-configuration' was given, "
        ++ "but the following flags were not specified: "
        ++ intercalate ", " (map show diffFlags)

-- | Create a PackageIndex that makes *any libraries that might be*
-- defined internally to this package look like installed packages, in
-- case an executable should refer to any of them as dependencies.
--
-- It must be *any libraries that might be* defined rather than the
-- actual definitions, because these depend on conditionals in the .cabal
-- file, and we haven't resolved them yet.  finalizePD
-- does the resolution of conditionals, and it takes internalPackageSet
-- as part of its input.
getInternalPackages :: GenericPackageDescription
                    -> Map PackageName (Maybe UnqualComponentName)
getInternalPackages pkg_descr0 =
    -- TODO: some day, executables will be fair game here too!
    let pkg_descr = flattenPackageDescription pkg_descr0
        f lib = case libName lib of
                    Nothing -> (packageName pkg_descr, Nothing)
                    Just n' -> (unqualComponentNameToPackageName n', Just n')
    in Map.fromList (map f (allLibraries pkg_descr))

-- | Returns true if a dependency is satisfiable.  This function may
-- report a dependency satisfiable even when it is not, but not vice
-- versa. This is to be passed to finalizePD.
dependencySatisfiable
    :: Bool -- ^ use external internal deps?
    -> Bool -- ^ exact configuration?
    -> PackageName
    -> InstalledPackageIndex -- ^ installed set
    -> Map PackageName (Maybe UnqualComponentName) -- ^ internal set
    -> Map PackageName InstalledPackageInfo -- ^ required dependencies
    -> (Dependency -> Bool)
dependencySatisfiable
  use_external_internal_deps
  exact_config pn installedPackageSet internalPackageSet requiredDepsMap
  d@(Dependency depName vr)

    | exact_config
    -- When we're given '--exact-configuration', we assume that all
    -- dependencies and flags are exactly specified on the command
    -- line. Thus we only consult the 'requiredDepsMap'. Note that
    -- we're not doing the version range check, so if there's some
    -- dependency that wasn't specified on the command line,
    -- 'finalizePD' will fail.
    -- TODO: mention '--exact-configuration' in the error message
    -- when this fails?
    = if isInternalDep && not use_external_internal_deps
        -- Except for internal deps, when we're NOT per-component mode;
        -- those are just True.
        then True
        else depName `Map.member` requiredDepsMap

    | isInternalDep
    = if use_external_internal_deps
        -- When we are doing per-component configure, we now need to
        -- test if the internal dependency is in the index.  This has
        -- DIFFERENT semantics from normal dependency satisfiability.
        then internalDepSatisfiable
        -- If a 'PackageName' is defined by an internal component, the dep is
        -- satisfiable (we're going to build it ourselves)
        else True

    | otherwise
    = depSatisfiable

  where
    isInternalDep = Map.member depName internalPackageSet

    depSatisfiable =
        not . null $ PackageIndex.lookupDependency installedPackageSet d

    internalDepSatisfiable =
        not . null $ PackageIndex.lookupInternalDependency
                        installedPackageSet (Dependency pn vr) cn
      where
        cn | pn == depName
           = Nothing
           | otherwise
           -- Reinterpret the "package name" as an unqualified component
           -- name
           = Just (mkUnqualComponentName (unPackageName depName))

-- | Finalize a generic package description.  The workhorse is
-- 'finalizePD' but there's a bit of other nattering
-- about necessary.
--
-- TODO: what exactly is the business with @flaggedTests@ and
-- @flaggedBenchmarks@?
configureFinalizedPackage
    :: Verbosity
    -> ConfigFlags
    -> ComponentRequestedSpec
    -> [Dependency]
    -> (Dependency -> Bool) -- ^ tests if a dependency is satisfiable.
                            -- Might say it's satisfiable even when not.
    -> Compiler
    -> Platform
    -> GenericPackageDescription
    -> IO (PackageDescription, FlagAssignment)
configureFinalizedPackage verbosity cfg enabled
  allConstraints satisfies comp compPlatform pkg_descr0 = do

    (pkg_descr0', flags) <-
            case finalizePD
                   (configConfigurationsFlags cfg)
                   enabled
                   satisfies
                   compPlatform
                   (compilerInfo comp)
                   allConstraints
                   pkg_descr0
            of Right r -> return r
               Left missing ->
                   die' verbosity $ "Encountered missing dependencies:\n"
                     ++ (render . nest 4 . sep . punctuate comma
                                . map (disp . simplifyDependency)
                                $ missing)

    -- add extra include/lib dirs as specified in cfg
    -- we do it here so that those get checked too
    let pkg_descr = addExtraIncludeLibDirs pkg_descr0'

    unless (nullFlagAssignment flags) $
      info verbosity $ "Flags chosen: "
                    ++ intercalate ", " [ unFlagName fn ++ "=" ++ display value
                                        | (fn, value) <- unFlagAssignment flags ]

    return (pkg_descr, flags)
  where
    addExtraIncludeLibDirs pkg_descr =
        let extraBi = mempty { extraLibDirs = configExtraLibDirs cfg
                             , extraFrameworkDirs = configExtraFrameworkDirs cfg
                             , PD.includeDirs = configExtraIncludeDirs cfg}
            modifyLib l        = l{ libBuildInfo = libBuildInfo l
                                                   `mappend` extraBi }
            modifyExecutable e = e{ buildInfo    = buildInfo e
                                                   `mappend` extraBi}
        in pkg_descr{ library = modifyLib `fmap` library pkg_descr
                    , subLibraries = modifyLib `map` subLibraries pkg_descr
                    , executables = modifyExecutable  `map`
                                      executables pkg_descr}

-- | Check for use of Cabal features which require compiler support
checkCompilerProblems :: Verbosity -> Compiler -> PackageDescription -> ComponentRequestedSpec -> IO ()
checkCompilerProblems verbosity comp pkg_descr enabled = do
    unless (renamingPackageFlagsSupported comp ||
                all (all (isDefaultIncludeRenaming . mixinIncludeRenaming) . mixins)
                         (enabledBuildInfos pkg_descr enabled)) $
        die' verbosity $ "Your compiler does not support thinning and renaming on "
           ++ "package flags.  To use this feature you must use "
           ++ "GHC 7.9 or later."

    when (any (not.null.PD.reexportedModules) (PD.allLibraries pkg_descr)
          && not (reexportedModulesSupported comp)) $
        die' verbosity $ "Your compiler does not support module re-exports. To use "
           ++ "this feature you must use GHC 7.9 or later."

    when (any (not.null.PD.signatures) (PD.allLibraries pkg_descr)
          && not (backpackSupported comp)) $
        die' verbosity $ "Your compiler does not support Backpack. To use "
           ++ "this feature you must use GHC 8.1 or later."

-- | Select dependencies for the package.
configureDependencies
    :: Verbosity
    -> UseExternalInternalDeps
    -> Map PackageName (Maybe UnqualComponentName) -- ^ internal packages
    -> InstalledPackageIndex -- ^ installed packages
    -> Map PackageName InstalledPackageInfo -- ^ required deps
    -> PackageDescription
    -> IO [PreExistingComponent]
configureDependencies verbosity use_external_internal_deps
  internalPackageSet installedPackageSet requiredDepsMap pkg_descr = do
    let failedDeps :: [FailedDependency]
        allPkgDeps :: [ResolvedDependency]
        (failedDeps, allPkgDeps) = partitionEithers
          [ (\s -> (dep, s)) <$> status
          | dep <- buildDepends pkg_descr
          , let status = selectDependency (package pkg_descr)
                  internalPackageSet installedPackageSet
                  requiredDepsMap use_external_internal_deps dep ]

        internalPkgDeps = [ pkgid
                          | (_, InternalDependency pkgid) <- allPkgDeps ]
        -- NB: we have to SAVE the package name, because this is the only
        -- way we can be able to resolve package names in the package
        -- description.
        externalPkgDeps = [ pec
                          | (_, ExternalDependency pec)   <- allPkgDeps ]

    when (not (null internalPkgDeps)
          && not (newPackageDepsBehaviour pkg_descr)) $
        die' verbosity $ "The field 'build-depends: "
           ++ intercalate ", " (map (display . packageName) internalPkgDeps)
           ++ "' refers to a library which is defined within the same "
           ++ "package. To use this feature the package must specify at "
           ++ "least 'cabal-version: >= 1.8'."

    reportFailedDependencies verbosity failedDeps
    reportSelectedDependencies verbosity allPkgDeps

    return externalPkgDeps

-- | Select and apply coverage settings for the build based on the
-- 'ConfigFlags' and 'Compiler'.
configureCoverage :: Verbosity -> ConfigFlags -> Compiler
                  -> IO (LocalBuildInfo -> LocalBuildInfo)
configureCoverage verbosity cfg comp = do
    let tryExeCoverage = fromFlagOrDefault False (configCoverage cfg)
        tryLibCoverage = fromFlagOrDefault tryExeCoverage
                         (mappend (configCoverage cfg) (configLibCoverage cfg))
    if coverageSupported comp
      then do
        let apply lbi = lbi { libCoverage = tryLibCoverage
                            , exeCoverage = tryExeCoverage
                            }
        return apply
      else do
        let apply lbi = lbi { libCoverage = False
                            , exeCoverage = False
                            }
        when (tryExeCoverage || tryLibCoverage) $ warn verbosity
          ("The compiler " ++ showCompilerId comp ++ " does not support "
           ++ "program coverage. Program coverage has been disabled.")
        return apply

-- | Compute the effective value of the profiling flags
-- @--enable-library-profiling@ and @--enable-executable-profiling@
-- from the specified 'ConfigFlags'.  This may be useful for
-- external Cabal tools which need to interact with Setup in
-- a backwards-compatible way: the most predictable mechanism
-- for enabling profiling across many legacy versions is to
-- NOT use @--enable-profiling@ and use those two flags instead.
--
-- Note that @--enable-executable-profiling@ also affects profiling
-- of benchmarks and (non-detailed) test suites.
computeEffectiveProfiling :: ConfigFlags -> (Bool {- lib -}, Bool {- exe -})
computeEffectiveProfiling cfg =
  -- The --profiling flag sets the default for both libs and exes,
  -- but can be overidden by --library-profiling, or the old deprecated
  -- --executable-profiling flag.
  --
  -- The --profiling-detail and --library-profiling-detail flags behave
  -- similarly
  let tryExeProfiling = fromFlagOrDefault False
                        (mappend (configProf cfg) (configProfExe cfg))
      tryLibProfiling = fromFlagOrDefault tryExeProfiling
                        (mappend (configProf cfg) (configProfLib cfg))
  in (tryLibProfiling, tryExeProfiling)

-- | Select and apply profiling settings for the build based on the
-- 'ConfigFlags' and 'Compiler'.
configureProfiling :: Verbosity -> ConfigFlags -> Compiler
                   -> IO (LocalBuildInfo -> LocalBuildInfo)
configureProfiling verbosity cfg comp = do
  let (tryLibProfiling, tryExeProfiling) = computeEffectiveProfiling cfg

      tryExeProfileLevel = fromFlagOrDefault ProfDetailDefault
                           (configProfDetail cfg)
      tryLibProfileLevel = fromFlagOrDefault ProfDetailDefault
                           (mappend
                            (configProfDetail cfg)
                            (configProfLibDetail cfg))

      checkProfileLevel (ProfDetailOther other) = do
        warn verbosity
          ("Unknown profiling detail level '" ++ other
           ++ "', using default.\nThe profiling detail levels are: "
           ++ intercalate ", "
           [ name | (name, _, _) <- knownProfDetailLevels ])
        return ProfDetailDefault
      checkProfileLevel other = return other

  (exeProfWithoutLibProf, applyProfiling) <-
    if profilingSupported comp
    then do
      exeLevel <- checkProfileLevel tryExeProfileLevel
      libLevel <- checkProfileLevel tryLibProfileLevel
      let apply lbi = lbi { withProfLib       = tryLibProfiling
                          , withProfLibDetail = libLevel
                          , withProfExe       = tryExeProfiling
                          , withProfExeDetail = exeLevel
                          }
      return (tryExeProfiling && not tryLibProfiling, apply)
    else do
      let apply lbi = lbi { withProfLib = False
                          , withProfLibDetail = ProfDetailNone
                          , withProfExe = False
                          , withProfExeDetail = ProfDetailNone
                          }
      when (tryExeProfiling || tryLibProfiling) $ warn verbosity
        ("The compiler " ++ showCompilerId comp ++ " does not support "
         ++ "profiling. Profiling has been disabled.")
      return (False, apply)

  when exeProfWithoutLibProf $ warn verbosity
    ("Executables will be built with profiling, but library "
     ++ "profiling is disabled. Linking will fail if any executables "
     ++ "depend on the library.")

  return applyProfiling

-- -----------------------------------------------------------------------------
-- Configuring package dependencies

reportProgram :: Verbosity -> Program -> Maybe ConfiguredProgram -> IO ()
reportProgram verbosity prog Nothing
    = info verbosity $ "No " ++ programName prog ++ " found"
reportProgram verbosity prog (Just configuredProg)
    = info verbosity $ "Using " ++ programName prog ++ version ++ location
    where location = case programLocation configuredProg of
            FoundOnSystem p -> " found on system at: " ++ p
            UserSpecified p -> " given by user at: " ++ p
          version = case programVersion configuredProg of
            Nothing -> ""
            Just v  -> " version " ++ display v

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org/package/"

type ResolvedDependency = (Dependency, DependencyResolution)

data DependencyResolution
    -- | An external dependency from the package database, OR an
    -- internal dependency which we are getting from the package
    -- database.
    = ExternalDependency PreExistingComponent
    -- | An internal dependency ('PackageId' should be a library name)
    -- which we are going to have to build.  (The
    -- 'PackageId' here is a hack to get a modest amount of
    -- polymorphism out of the 'Package' typeclass.)
    | InternalDependency PackageId

data FailedDependency = DependencyNotExists PackageName
                      | DependencyMissingInternal PackageName PackageName
                      | DependencyNoVersion Dependency

-- | Test for a package dependency and record the version we have installed.
selectDependency :: PackageId -- ^ Package id of current package
                 -> Map PackageName (Maybe UnqualComponentName)
                 -> InstalledPackageIndex  -- ^ Installed packages
                 -> Map PackageName InstalledPackageInfo
                    -- ^ Packages for which we have been given specific deps to
                    -- use
                 -> UseExternalInternalDeps -- ^ Are we configuring a
                                            -- single component?
                 -> Dependency
                 -> Either FailedDependency DependencyResolution
selectDependency pkgid internalIndex installedIndex requiredDepsMap
  use_external_internal_deps
  dep@(Dependency dep_pkgname vr) =
  -- If the dependency specification matches anything in the internal package
  -- index, then we prefer that match to anything in the second.
  -- For example:
  --
  -- Name: MyLibrary
  -- Version: 0.1
  -- Library
  --     ..
  -- Executable my-exec
  --     build-depends: MyLibrary
  --
  -- We want "build-depends: MyLibrary" always to match the internal library
  -- even if there is a newer installed library "MyLibrary-0.2".
  case Map.lookup dep_pkgname internalIndex of
    Just cname -> if use_external_internal_deps
                    then do_external (Just cname)
                    else do_internal
    _          -> do_external Nothing
  where

    -- It's an internal library, and we're not per-component build
    do_internal = Right $ InternalDependency
                    $ PackageIdentifier dep_pkgname $ packageVersion pkgid

    -- We have to look it up externally
    do_external is_internal = do
      ipi <- case Map.lookup dep_pkgname requiredDepsMap of
        -- If we know the exact pkg to use, then use it.
        Just pkginstance -> Right pkginstance
        -- Otherwise we just pick an arbitrary instance of the latest version.
        Nothing ->
            case is_internal of
                Nothing     -> do_external_external
                Just mb_uqn -> do_external_internal mb_uqn
      return $ ExternalDependency $ ipiToPreExistingComponent ipi

    -- It's an external package, normal situation
    do_external_external =
        case PackageIndex.lookupDependency installedIndex dep of
          []   -> Left (DependencyNotExists dep_pkgname)
          pkgs -> Right $ head $ snd $ last pkgs

    -- It's an internal library, being looked up externally
    do_external_internal mb_uqn =
        case PackageIndex.lookupInternalDependency installedIndex
                (Dependency (packageName pkgid) vr) mb_uqn of
          []   -> Left (DependencyMissingInternal dep_pkgname (packageName pkgid))
          pkgs -> Right $ head $ snd $ last pkgs

reportSelectedDependencies :: Verbosity
                           -> [ResolvedDependency] -> IO ()
reportSelectedDependencies verbosity deps =
  info verbosity $ unlines
    [ "Dependency " ++ display (simplifyDependency dep)
                    ++ ": using " ++ display pkgid
    | (dep, resolution) <- deps
    , let pkgid = case resolution of
            ExternalDependency pkg'   -> packageId pkg'
            InternalDependency pkgid' -> pkgid' ]

reportFailedDependencies :: Verbosity -> [FailedDependency] -> IO ()
reportFailedDependencies _ []     = return ()
reportFailedDependencies verbosity failed =
    die' verbosity (intercalate "\n\n" (map reportFailedDependency failed))

  where
    reportFailedDependency (DependencyNotExists pkgname) =
         "there is no version of " ++ display pkgname ++ " installed.\n"
      ++ "Perhaps you need to download and install it from\n"
      ++ hackageUrl ++ display pkgname ++ "?"

    reportFailedDependency (DependencyMissingInternal pkgname real_pkgname) =
         "internal dependency " ++ display pkgname ++ " not installed.\n"
      ++ "Perhaps you need to configure and install it first?\n"
      ++ "(This library was defined by " ++ display real_pkgname ++ ")"

    reportFailedDependency (DependencyNoVersion dep) =
        "cannot satisfy dependency " ++ display (simplifyDependency dep) ++ "\n"

-- | List all installed packages in the given package databases.
getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -- ^ The stack of package databases.
                     -> ProgramDb
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packageDBs progdb = do
  when (null packageDBs) $
    die' verbosity $ "No package databases have been specified. If you use "
       ++ "--package-db=clear, you must follow it with --package-db= "
       ++ "with 'global', 'user' or a specific file."

  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC   -> GHC.getInstalledPackages verbosity comp packageDBs progdb
    GHCJS -> GHCJS.getInstalledPackages verbosity packageDBs progdb
    JHC   -> JHC.getInstalledPackages verbosity packageDBs progdb
    LHC   -> LHC.getInstalledPackages verbosity packageDBs progdb
    UHC   -> UHC.getInstalledPackages verbosity comp packageDBs progdb
    HaskellSuite {} ->
      HaskellSuite.getInstalledPackages verbosity packageDBs progdb
    flv -> die' verbosity $ "don't know how to find the installed packages for "
              ++ display flv

-- | Like 'getInstalledPackages', but for a single package DB.
--
-- NB: Why isn't this always a fall through to 'getInstalledPackages'?
-- That is because 'getInstalledPackages' performs some sanity checks
-- on the package database stack in question.  However, when sandboxes
-- are involved these sanity checks are not desirable.
getPackageDBContents :: Verbosity -> Compiler
                     -> PackageDB -> ProgramDb
                     -> IO InstalledPackageIndex
getPackageDBContents verbosity comp packageDB progdb = do
  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC -> GHC.getPackageDBContents verbosity packageDB progdb
    GHCJS -> GHCJS.getPackageDBContents verbosity packageDB progdb
    -- For other compilers, try to fall back on 'getInstalledPackages'.
    _   -> getInstalledPackages verbosity comp [packageDB] progdb


-- | A set of files (or directories) that can be monitored to detect when
-- there might have been a change in the installed packages.
--
getInstalledPackagesMonitorFiles :: Verbosity -> Compiler
                                 -> PackageDBStack
                                 -> ProgramDb -> Platform
                                 -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity comp packageDBs progdb platform =
  case compilerFlavor comp of
    GHC   -> GHC.getInstalledPackagesMonitorFiles
               verbosity platform progdb packageDBs
    other -> do
      warn verbosity $ "don't know how to find change monitoring files for "
                    ++ "the installed package databases for " ++ display other
      return []

-- | The user interface specifies the package dbs to use with a combination of
-- @--global@, @--user@ and @--package-db=global|user|clear|$file@.
-- This function combines the global/user flag and interprets the package-db
-- flag into a single package db stack.
--
interpretPackageDbFlags :: Bool -> [Maybe PackageDB] -> PackageDBStack
interpretPackageDbFlags userInstall specificDBs =
    extra initialStack specificDBs
  where
    initialStack | userInstall = [GlobalPackageDB, UserPackageDB]
                 | otherwise   = [GlobalPackageDB]

    extra dbs' []            = dbs'
    extra _    (Nothing:dbs) = extra []             dbs
    extra dbs' (Just db:dbs) = extra (dbs' ++ [db]) dbs

-- We are given both --constraint="foo < 2.0" style constraints and also
-- specific packages to pick via --dependency="foo=foo-2.0-177d5cdf20962d0581".
--
-- When finalising the package we have to take into account the specific
-- installed deps we've been given, and the finalise function expects
-- constraints, so we have to translate these deps into version constraints.
--
-- But after finalising we then have to make sure we pick the right specific
-- deps in the end. So we still need to remember which installed packages to
-- pick.
combinedConstraints :: [Dependency] ->
                       [(PackageName, ComponentId)] ->
                       InstalledPackageIndex ->
                       Either String ([Dependency],
                                      Map PackageName InstalledPackageInfo)
combinedConstraints constraints dependencies installedPackages = do

    when (not (null badComponentIds)) $
      Left $ render $ text "The following package dependencies were requested"
         $+$ nest 4 (dispDependencies badComponentIds)
         $+$ text "however the given installed package instance does not exist."

    --TODO: we don't check that all dependencies are used!

    return (allConstraints, idConstraintMap)

  where
    allConstraints :: [Dependency]
    allConstraints = constraints
                  ++ [ thisPackageVersion (packageId pkg)
                     | (_, _, Just pkg) <- dependenciesPkgInfo ]

    idConstraintMap :: Map PackageName InstalledPackageInfo
    idConstraintMap = Map.fromList
                        -- NB: do NOT use the packageName from
                        -- dependenciesPkgInfo!
                        [ (pn, pkg)
                        | (pn, _, Just pkg) <- dependenciesPkgInfo ]

    -- The dependencies along with the installed package info, if it exists
    dependenciesPkgInfo :: [(PackageName, ComponentId,
                             Maybe InstalledPackageInfo)]
    dependenciesPkgInfo =
      [ (pkgname, cid, mpkg)
      | (pkgname, cid) <- dependencies
      , let mpkg = PackageIndex.lookupComponentId
                     installedPackages cid
      ]

    -- If we looked up a package specified by an installed package id
    -- (i.e. someone has written a hash) and didn't find it then it's
    -- an error.
    badComponentIds =
      [ (pkgname, cid)
      | (pkgname, cid, Nothing) <- dependenciesPkgInfo ]

    dispDependencies deps =
      hsep [    text "--dependency="
             <<>> quotes (disp pkgname <<>> char '=' <<>> disp cid)
           | (pkgname, cid) <- deps ]

-- -----------------------------------------------------------------------------
-- Configuring program dependencies

configureRequiredPrograms :: Verbosity -> [LegacyExeDependency] -> ProgramDb
                             -> IO ProgramDb
configureRequiredPrograms verbosity deps progdb =
  foldM (configureRequiredProgram verbosity) progdb deps

-- | Configure a required program, ensuring that it exists in the PATH
-- (or where the user has specified the program must live) and making it
-- available for use via the 'ProgramDb' interface.  If the program is
-- known (exists in the input 'ProgramDb'), we will make sure that the
-- program matches the required version; otherwise we will accept
-- any version of the program and assume that it is a simpleProgram.
configureRequiredProgram :: Verbosity -> ProgramDb -> LegacyExeDependency
                            -> IO ProgramDb
configureRequiredProgram verbosity progdb
  (LegacyExeDependency progName verRange) =
  case lookupKnownProgram progName progdb of
    Nothing ->
      -- Try to configure it as a 'simpleProgram' automatically
      --
      -- There's a bit of a story behind this line.  In old versions
      -- of Cabal, there were only internal build-tools dependencies.  So the
      -- behavior in this case was:
      --
      --    - If a build-tool dependency was internal, don't do
      --      any checking.
      --
      --    - If it was external, call 'configureRequiredProgram' to
      --      "configure" the executable.  In particular, if
      --      the program was not "known" (present in 'ProgramDb'),
      --      then we would just error.  This was fine, because
      --      the only way a program could be executed from 'ProgramDb'
      --      is if some library code from Cabal actually called it,
      --      and the pre-existing Cabal code only calls known
      --      programs from 'defaultProgramDb', and so if it
      --      is calling something else, you have a Custom setup
      --      script, and in that case you are expected to register
      --      the program you want to call in the ProgramDb.
      --
      -- OK, so that was fine, until I (ezyang, in 2016) refactored
      -- Cabal to support per-component builds.  In this case, what
      -- was previously an internal build-tool dependency now became
      -- an external one, and now previously "internal" dependencies
      -- are now external.  But these are permitted to exist even
      -- when they are not previously configured (something that
      -- can only occur by a Custom script.)
      --
      -- So, I decided, "Fine, let's just accept these in any
      -- case."  Thus this line.  The alternative would have been to
      -- somehow detect when a build-tools dependency was "internal" (by
      -- looking at the unflattened package description) but this
      -- would also be incompatible with future work to support
      -- external executable dependencies: we definitely cannot
      -- assume they will be preinitialized in the 'ProgramDb'.
      configureProgram verbosity (simpleProgram progName) progdb
    Just prog
      -- requireProgramVersion always requires the program have a version
      -- but if the user says "build-depends: foo" ie no version constraint
      -- then we should not fail if we cannot discover the program version.
      | verRange == anyVersion -> do
          (_, progdb') <- requireProgram verbosity prog progdb
          return progdb'
      | otherwise -> do
          (_, _, progdb') <- requireProgramVersion verbosity prog verRange progdb
          return progdb'

-- -----------------------------------------------------------------------------
-- Configuring pkg-config package dependencies

configurePkgconfigPackages :: Verbosity -> PackageDescription
                           -> ProgramDb -> ComponentRequestedSpec
                           -> IO (PackageDescription, ProgramDb)
configurePkgconfigPackages verbosity pkg_descr progdb enabled
  | null allpkgs = return (pkg_descr, progdb)
  | otherwise    = do
    (_, _, progdb') <- requireProgramVersion
                       (lessVerbose verbosity) pkgConfigProgram
                       (orLaterVersion $ mkVersion [0,9,0]) progdb
    traverse_ requirePkg allpkgs
    mlib' <- traverse addPkgConfigBILib (library pkg_descr)
    libs' <- traverse addPkgConfigBILib (subLibraries pkg_descr)
    exes' <- traverse addPkgConfigBIExe (executables pkg_descr)
    tests' <- traverse addPkgConfigBITest (testSuites pkg_descr)
    benches' <- traverse addPkgConfigBIBench (benchmarks pkg_descr)
    let pkg_descr' = pkg_descr { library = mlib',
                                 subLibraries = libs', executables = exes',
                                 testSuites = tests', benchmarks = benches' }
    return (pkg_descr', progdb')

  where
    allpkgs = concatMap pkgconfigDepends (enabledBuildInfos pkg_descr enabled)
    pkgconfig = getDbProgramOutput (lessVerbose verbosity)
                  pkgConfigProgram progdb

    requirePkg dep@(PkgconfigDependency pkgn range) = do
      version <- pkgconfig ["--modversion", pkg]
                 `catchIO`   (\_ -> die' verbosity notFound)
                 `catchExit` (\_ -> die' verbosity notFound)
      case simpleParse version of
        Nothing -> die' verbosity "parsing output of pkg-config --modversion failed"
        Just v | not (withinRange v range) -> die' verbosity (badVersion v)
               | otherwise                 -> info verbosity (depSatisfied v)
      where
        notFound     = "The pkg-config package '" ++ pkg ++ "'"
                    ++ versionRequirement
                    ++ " is required but it could not be found."
        badVersion v = "The pkg-config package '" ++ pkg ++ "'"
                    ++ versionRequirement
                    ++ " is required but the version installed on the"
                    ++ " system is version " ++ display v
        depSatisfied v = "Dependency " ++ display dep
                      ++ ": using version " ++ display v

        versionRequirement
          | isAnyVersion range = ""
          | otherwise          = " version " ++ display range

        pkg = unPkgconfigName pkgn

    -- Adds pkgconfig dependencies to the build info for a component
    addPkgConfigBI compBI setCompBI comp = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (compBI comp))
      return $ setCompBI comp (compBI comp `mappend` bi)

    -- Adds pkgconfig dependencies to the build info for a library
    addPkgConfigBILib = addPkgConfigBI libBuildInfo $
                          \lib bi -> lib { libBuildInfo = bi }

    -- Adds pkgconfig dependencies to the build info for an executable
    addPkgConfigBIExe = addPkgConfigBI buildInfo $
                          \exe bi -> exe { buildInfo = bi }

    -- Adds pkgconfig dependencies to the build info for a test suite
    addPkgConfigBITest = addPkgConfigBI testBuildInfo $
                          \test bi -> test { testBuildInfo = bi }

    -- Adds pkgconfig dependencies to the build info for a benchmark
    addPkgConfigBIBench = addPkgConfigBI benchmarkBuildInfo $
                          \bench bi -> bench { benchmarkBuildInfo = bi }

    pkgconfigBuildInfo :: [PkgconfigDependency] -> NoCallStackIO BuildInfo
    pkgconfigBuildInfo []      = return mempty
    pkgconfigBuildInfo pkgdeps = do
      let pkgs = nub [ display pkg | PkgconfigDependency pkg _ <- pkgdeps ]
      ccflags <- pkgconfig ("--cflags" : pkgs)
      ldflags <- pkgconfig ("--libs"   : pkgs)
      return (ccLdOptionsBuildInfo (words ccflags) (words ldflags))

-- | Makes a 'BuildInfo' from C compiler and linker flags.
--
-- This can be used with the output from configuration programs like pkg-config
-- and similar package-specific programs like mysql-config, freealut-config etc.
-- For example:
--
-- > ccflags <- getDbProgramOutput verbosity prog progdb ["--cflags"]
-- > ldflags <- getDbProgramOutput verbosity prog progdb ["--libs"]
-- > return (ccldOptionsBuildInfo (words ccflags) (words ldflags))
--
ccLdOptionsBuildInfo :: [String] -> [String] -> BuildInfo
ccLdOptionsBuildInfo cflags ldflags =
  let (includeDirs',  cflags')   = partition ("-I" `isPrefixOf`) cflags
      (extraLibs',    ldflags')  = partition ("-l" `isPrefixOf`) ldflags
      (extraLibDirs', ldflags'') = partition ("-L" `isPrefixOf`) ldflags'
  in mempty {
       PD.includeDirs  = map (drop 2) includeDirs',
       PD.extraLibs    = map (drop 2) extraLibs',
       PD.extraLibDirs = map (drop 2) extraLibDirs',
       PD.ccOptions    = cflags',
       PD.ldOptions    = ldflags''
     }

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAuxEx :: ConfigFlags
                    -> IO (Compiler, Platform, ProgramDb)
configCompilerAuxEx cfg = configCompilerEx (flagToMaybe $ configHcFlavor cfg)
                                           (flagToMaybe $ configHcPath cfg)
                                           (flagToMaybe $ configHcPkg cfg)
                                           programDb
                                           (fromFlag (configVerbosity cfg))
  where
    programDb = mkProgramDb cfg defaultProgramDb

configCompilerEx :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
                 -> ProgramDb -> Verbosity
                 -> IO (Compiler, Platform, ProgramDb)
configCompilerEx Nothing _ _ _ verbosity = die' verbosity "Unknown compiler"
configCompilerEx (Just hcFlavor) hcPath hcPkg progdb verbosity = do
  (comp, maybePlatform, programDb) <- case hcFlavor of
    GHC   -> GHC.configure  verbosity hcPath hcPkg progdb
    GHCJS -> GHCJS.configure verbosity hcPath hcPkg progdb
    JHC   -> JHC.configure  verbosity hcPath hcPkg progdb
    LHC   -> do (_, _, ghcConf) <- GHC.configure  verbosity Nothing hcPkg progdb
                LHC.configure  verbosity hcPath Nothing ghcConf
    UHC   -> UHC.configure  verbosity hcPath hcPkg progdb
    HaskellSuite {} -> HaskellSuite.configure verbosity hcPath hcPkg progdb
    _    -> die' verbosity "Unknown compiler"
  return (comp, fromMaybe buildPlatform maybePlatform, programDb)

-- Ideally we would like to not have separate configCompiler* and
-- configCompiler*Ex sets of functions, but there are many custom setup scripts
-- in the wild that are using them, so the versions with old types are kept for
-- backwards compatibility. Platform was added to the return triple in 1.18.

{-# DEPRECATED configCompiler
    "'configCompiler' is deprecated. Use 'configCompilerEx' instead." #-}
configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
               -> ProgramDb -> Verbosity
               -> IO (Compiler, ProgramDb)
configCompiler mFlavor hcPath hcPkg progdb verbosity =
  fmap (\(a,_,b) -> (a,b)) $ configCompilerEx mFlavor hcPath hcPkg progdb verbosity

{-# DEPRECATED configCompilerAux
    "configCompilerAux is deprecated. Use 'configCompilerAuxEx' instead." #-}
configCompilerAux :: ConfigFlags
                  -> IO (Compiler, ProgramDb)
configCompilerAux = fmap (\(a,_,b) -> (a,b)) . configCompilerAuxEx

-- -----------------------------------------------------------------------------
-- Testing C lib and header dependencies

-- Try to build a test C program which includes every header and links every
-- lib. If that fails, try to narrow it down by preprocessing (only) and linking
-- with individual headers and libs.  If none is the obvious culprit then give a
-- generic error message.
-- TODO: produce a log file from the compiler errors, if any.
checkForeignDeps :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
checkForeignDeps pkg lbi verbosity =
  ifBuildsWith allHeaders (commonCcArgs ++ makeLdArgs allLibs) -- I'm feeling
                                                               -- lucky
           (return ())
           (do missingLibs <- findMissingLibs
               missingHdr  <- findOffendingHdr
               explainErrors missingHdr missingLibs)
      where
        allHeaders = collectField PD.includes
        allLibs    = collectField PD.extraLibs

        ifBuildsWith headers args success failure = do
            ok <- builds (makeProgram headers) args
            if ok then success else failure

        findOffendingHdr =
            ifBuildsWith allHeaders ccArgs
                         (return Nothing)
                         (go . tail . inits $ allHeaders)
            where
              go [] = return Nothing       -- cannot happen
              go (hdrs:hdrsInits) =
                    -- Try just preprocessing first
                    ifBuildsWith hdrs cppArgs
                      -- If that works, try compiling too
                      (ifBuildsWith hdrs ccArgs
                        (go hdrsInits)
                        (return . Just . Right . last $ hdrs))
                      (return . Just . Left . last $ hdrs)

              cppArgs = "-E":commonCppArgs -- preprocess only
              ccArgs  = "-c":commonCcArgs  -- don't try to link

        findMissingLibs = ifBuildsWith [] (makeLdArgs allLibs)
                                       (return [])
                                       (filterM (fmap not . libExists) allLibs)

        libExists lib = builds (makeProgram []) (makeLdArgs [lib])

        commonCppArgs = platformDefines lbi
                     -- TODO: This is a massive hack, to work around the
                     -- fact that the test performed here should be
                     -- PER-component (c.f. the "I'm Feeling Lucky"; we
                     -- should NOT be glomming everything together.)
                     ++ [ "-I" ++ buildDir lbi </> "autogen" ]
                     ++ [ "-I" ++ dir | dir <- collectField PD.includeDirs ]
                     ++ ["-I."]
                     ++ collectField PD.cppOptions
                     ++ collectField PD.ccOptions
                     ++ [ "-I" ++ dir
                        | dir <- ordNub [ dir
                                        | dep <- deps
                                        , dir <- Installed.includeDirs dep ]
                                 -- dedupe include dirs of dependencies
                                 -- to prevent quadratic blow-up
                        ]
                     ++ [ opt
                        | dep <- deps
                        , opt <- Installed.ccOptions dep ]

        commonCcArgs  = commonCppArgs
                     ++ collectField PD.ccOptions
                     ++ [ opt
                        | dep <- deps
                        , opt <- Installed.ccOptions dep ]

        commonLdArgs  = [ "-L" ++ dir | dir <- collectField PD.extraLibDirs ]
                     ++ collectField PD.ldOptions
                     ++ [ "-L" ++ dir
                        | dir <- ordNub [ dir
                                        | dep <- deps
                                        , dir <- Installed.libraryDirs dep ]
                        ]
                     --TODO: do we also need dependent packages' ld options?
        makeLdArgs libs = [ "-l"++lib | lib <- libs ] ++ commonLdArgs

        makeProgram hdrs = unlines $
                           [ "#include \""  ++ hdr ++ "\"" | hdr <- hdrs ] ++
                           ["int main(int argc, char** argv) { return 0; }"]

        collectField f = concatMap f allBi
        allBi = enabledBuildInfos pkg (componentEnabledSpec lbi)
        deps = PackageIndex.topologicalOrder (installedPkgs lbi)

        builds program args = do
            tempDir <- getTemporaryDirectory
            withTempFile tempDir ".c" $ \cName cHnd ->
              withTempFile tempDir "" $ \oNname oHnd -> do
                hPutStrLn cHnd program
                hClose cHnd
                hClose oHnd
                _ <- getDbProgramOutput verbosity
                  gccProgram (withPrograms lbi) (cName:"-o":oNname:args)
                return True
           `catchIO`   (\_ -> return False)
           `catchExit` (\_ -> return False)

        explainErrors Nothing [] = return () -- should be impossible!
        explainErrors _ _
           | isNothing . lookupProgram gccProgram . withPrograms $ lbi

                              = die' verbosity $ unlines
              [ "No working gcc",
                  "This package depends on foreign library but we cannot "
               ++ "find a working C compiler. If you have it in a "
               ++ "non-standard location you can use the --with-gcc "
               ++ "flag to specify it." ]

        explainErrors hdr libs = die' verbosity $ unlines $
             [ if plural
                 then "Missing dependencies on foreign libraries:"
                 else "Missing dependency on a foreign library:"
             | missing ]
          ++ case hdr of
               Just (Left h) -> ["* Missing (or bad) header file: " ++ h ]
               _             -> []
          ++ case libs of
               []    -> []
               [lib] -> ["* Missing (or bad) C library: " ++ lib]
               _     -> ["* Missing (or bad) C libraries: " ++ intercalate ", " libs]
          ++ [if plural then messagePlural else messageSingular | missing]
          ++ case hdr of
               Just (Left  _) -> [ headerCppMessage ]
               Just (Right h) -> [ (if missing then "* " else "")
                                   ++ "Bad header file: " ++ h
                                 , headerCcMessage ]
               _              -> []

          where
            plural  = length libs >= 2
            -- Is there something missing? (as opposed to broken)
            missing = not (null libs)
                   || case hdr of Just (Left _) -> True; _ -> False

        messageSingular =
             "This problem can usually be solved by installing the system "
          ++ "package that provides this library (you may need the "
          ++ "\"-dev\" version). If the library is already installed "
          ++ "but in a non-standard location then you can use the flags "
          ++ "--extra-include-dirs= and --extra-lib-dirs= to specify "
          ++ "where it is."
          ++ "If the library file does exist, it may contain errors that "
          ++ "are caught by the C compiler at the preprocessing stage. "
          ++ "In this case you can re-run configure with the verbosity "
          ++ "flag -v3 to see the error messages."
        messagePlural =
             "This problem can usually be solved by installing the system "
          ++ "packages that provide these libraries (you may need the "
          ++ "\"-dev\" versions). If the libraries are already installed "
          ++ "but in a non-standard location then you can use the flags "
          ++ "--extra-include-dirs= and --extra-lib-dirs= to specify "
          ++ "where they are."
          ++ "If the library files do exist, it may contain errors that "
          ++ "are caught by the C compiler at the preprocessing stage. "
          ++ "In this case you can re-run configure with the verbosity "
          ++ "flag -v3 to see the error messages."
        headerCppMessage =
             "If the header file does exist, it may contain errors that "
          ++ "are caught by the C compiler at the preprocessing stage. "
          ++ "In this case you can re-run configure with the verbosity "
          ++ "flag -v3 to see the error messages."
        headerCcMessage =
             "The header file contains a compile error. "
          ++ "You can re-run configure with the verbosity flag "
          ++ "-v3 to see the error messages from the C compiler."

-- | Output package check warnings and errors. Exit if any errors.
checkPackageProblems :: Verbosity
                     -> GenericPackageDescription
                     -> PackageDescription
                     -> IO ()
checkPackageProblems verbosity gpkg pkg = do
  ioChecks      <- checkPackageFiles pkg "."
  let pureChecks = checkPackage gpkg (Just pkg)
      errors   = [ e | PackageBuildImpossible e <- pureChecks ++ ioChecks ]
      warnings = [ w | PackageBuildWarning    w <- pureChecks ++ ioChecks ]
  if null errors
    then traverse_ (warn verbosity) warnings
    else die' verbosity (intercalate "\n\n" errors)

-- | Preform checks if a relocatable build is allowed
checkRelocatable :: Verbosity
                 -> PackageDescription
                 -> LocalBuildInfo
                 -> IO ()
checkRelocatable verbosity pkg lbi
    = sequence_ [ checkOS
                , checkCompiler
                , packagePrefixRelative
                , depsPrefixRelative
                ]
  where
    -- Check if the OS support relocatable builds.
    --
    -- If you add new OS' to this list, and your OS supports dynamic libraries
    -- and RPATH, make sure you add your OS to RPATH-support list of:
    -- Distribution.Simple.GHC.getRPaths
    checkOS
        = unless (os `elem` [ OSX, Linux ])
        $ die' verbosity $ "Operating system: " ++ display os ++
                ", does not support relocatable builds"
      where
        (Platform _ os) = hostPlatform lbi

    -- Check if the Compiler support relocatable builds
    checkCompiler
        = unless (compilerFlavor comp `elem` [ GHC ])
        $ die' verbosity $ "Compiler: " ++ show comp ++
                ", does not support relocatable builds"
      where
        comp = compiler lbi

    -- Check if all the install dirs are relative to same prefix
    packagePrefixRelative
        = unless (relativeInstallDirs installDirs)
        $ die' verbosity $ "Installation directories are not prefix_relative:\n" ++
                show installDirs
      where
        -- NB: should be good enough to check this against the default
        -- component ID, but if we wanted to be strictly correct we'd
        -- check for each ComponentId.
        installDirs = absoluteInstallDirs pkg lbi NoCopyDest
        p           = prefix installDirs
        relativeInstallDirs (InstallDirs {..}) =
          all isJust
              (fmap (stripPrefix p)
                    [ bindir, libdir, dynlibdir, libexecdir, includedir, datadir
                    , docdir, mandir, htmldir, haddockdir, sysconfdir] )

    -- Check if the library dirs of the dependencies that are in the package
    -- database to which the package is installed are relative to the
    -- prefix of the package
    depsPrefixRelative = do
        pkgr <- GHC.pkgRoot verbosity lbi (last (withPackageDB lbi))
        traverse_ (doCheck pkgr) ipkgs
      where
        doCheck pkgr ipkg
          | maybe False (== pkgr) (Installed.pkgRoot ipkg)
          = traverse_ (\l -> when (isNothing $ stripPrefix p l) (die' verbosity (msg l)))
                  (Installed.libraryDirs ipkg)
          | otherwise
          = return ()
        -- NB: should be good enough to check this against the default
        -- component ID, but if we wanted to be strictly correct we'd
        -- check for each ComponentId.
        installDirs   = absoluteInstallDirs pkg lbi NoCopyDest
        p             = prefix installDirs
        ipkgs         = PackageIndex.allPackages (installedPkgs lbi)
        msg l         = "Library directory of a dependency: " ++ show l ++
                        "\nis not relative to the installation prefix:\n" ++
                        show p

-- -----------------------------------------------------------------------------
-- Testing foreign library requirements

unsupportedForeignLibs :: Compiler -> Platform -> [ForeignLib] -> [String]
unsupportedForeignLibs comp platform =
    mapMaybe (checkForeignLibSupported comp platform)

checkForeignLibSupported :: Compiler -> Platform -> ForeignLib -> Maybe String
checkForeignLibSupported comp platform flib = go (compilerFlavor comp)
  where
    go :: CompilerFlavor -> Maybe String
    go GHC
      | compilerVersion comp < mkVersion [7,8] = unsupported [
        "Building foreign libraires is only supported with GHC >= 7.8"
      ]
      | otherwise = goGhcPlatform platform
    go _   = unsupported [
        "Building foreign libraries is currently only supported with ghc"
      ]

    goGhcPlatform :: Platform -> Maybe String
    goGhcPlatform (Platform X86_64 OSX    ) = goGhcOsx     (foreignLibType flib)
    goGhcPlatform (Platform I386   Linux  ) = goGhcLinux   (foreignLibType flib)
    goGhcPlatform (Platform X86_64 Linux  ) = goGhcLinux   (foreignLibType flib)
    goGhcPlatform (Platform I386   Windows) = goGhcWindows (foreignLibType flib)
    goGhcPlatform (Platform X86_64 Windows) = goGhcWindows (foreignLibType flib)
    goGhcPlatform _ = unsupported [
        "Building foreign libraries is currently only supported on OSX, "
      , "Linux and Windows"
      ]

    goGhcOsx :: ForeignLibType -> Maybe String
    goGhcOsx ForeignLibNativeShared
      | standalone = unsupported [
            "We cannot build standalone libraries on OSX"
          ]
      | not (null (foreignLibModDefFile flib)) = unsupported [
            "Module definition file not supported on OSX"
          ]
      | not (null (foreignLibVersionInfo flib)) = unsupported [
            "Foreign library versioning not currently supported on OSX"
          ]
      | otherwise =
          Nothing
    goGhcOsx _ = unsupported [
        "We can currently only build shared foreign libraries on OSX"
      ]

    goGhcLinux :: ForeignLibType -> Maybe String
    goGhcLinux ForeignLibNativeShared
      | standalone = unsupported [
            "We cannot build standalone libraries on Linux"
          ]
      | not (null (foreignLibModDefFile flib)) = unsupported [
            "Module definition file not supported on Linux"
          ]
      | not (null (foreignLibVersionInfo flib))
          && not (null (foreignLibVersionLinux flib)) = unsupported [
            "You must not specify both lib-version-info and lib-version-linux"
          ]
      | otherwise =
          Nothing
    goGhcLinux _ = unsupported [
        "We can currently only build shared foreign libraries on Linux"
      ]

    goGhcWindows :: ForeignLibType -> Maybe String
    goGhcWindows ForeignLibNativeShared
      | not standalone = unsupported [
            "We can currently only build standalone libraries on Windows. Use\n"
          , "  if os(Windows)\n"
          , "    options: standalone\n"
          , "in your foreign-library stanza."
          ]
      | not (null (foreignLibVersionInfo flib)) = unsupported [
            "Foreign library versioning not currently supported on Windows.\n"
          , "You can specify module definition files in the mod-def-file field."
          ]
      | otherwise =
         Nothing
    goGhcWindows _ = unsupported [
        "We can currently only build shared foreign libraries on Windows"
      ]

    standalone :: Bool
    standalone = ForeignLibStandalone `elem` foreignLibOptions flib

    unsupported :: [String] -> Maybe String
    unsupported = Just . concat
