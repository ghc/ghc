{-# LANGUAGE TypeApplications #-}
module Rules.Register (
    configurePackageRules, registerPackageRules, registerPackages,
    libraryTargets
    ) where

import Base
import Context
import Expression ( getContextData )
import Oracles.Setting
import Hadrian.BuildPath
import Hadrian.Expression
import Hadrian.Haskell.Cabal
import Oracles.Flag (targetSupportsGhciObjects)
import Packages
import Rules.Rts
import Settings
import Target
import Utilities

import Hadrian.Haskell.Cabal.Type
import qualified Text.Parsec      as Parsec
import qualified Data.Set         as Set
import qualified Data.Char        as Char
import Data.Bifunctor (bimap)

import Distribution.Version (Version)
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Compat.Parsing as Cabal
import qualified Distribution.Parsec.FieldLineStream as Cabal
import qualified Distribution.Compat.CharParsing as CabalCharParsing

import qualified Hadrian.Haskell.Cabal.Parse as Cabal
import qualified System.Directory            as IO

-- * Configuring

-- | Configure a package and build its @setup-config@ file, as well as files in
-- the @build/pkgName/build/autogen@ directory.
configurePackageRules :: Rules ()
configurePackageRules = do
    root <- buildRootRules
    root -/- "**/setup-config" %> \out -> do
        (stage, path) <- parsePath (parseSetupConfig root) "<setup config path parser>" out
        let pkg = unsafeFindPackageByPath path
        let ctx = Context stage pkg vanilla Inplace
        buildP <- buildPath ctx
        when (pkg == ghcInternal) $ do
          isGmp <- (== "gmp") <$> interpretInContext ctx getBignumBackend
          when isGmp $
            need [buildP -/- "include/ghc-gmp.h"]
        Cabal.configurePackage ctx

    root -/- "**/autogen/cabal_macros.h" %> \out -> do
        (stage, path) <- parsePath (parseToBuildSubdirectory root) "<cabal macros path parser>" out
        let pkg = unsafeFindPackageByPath path
        Cabal.buildAutogenFiles (Context stage pkg vanilla Inplace)

    root -/- "**/autogen/Paths_*.hs" %> \out ->
        need [takeDirectory out -/- "cabal_macros.h"]

parseSetupConfig :: FilePath -> Parsec.Parsec String () (Stage, FilePath)
parseSetupConfig root = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgPath <- Parsec.manyTill Parsec.anyChar
        (Parsec.try $ Parsec.string "/setup-config")
    return (stage, pkgPath)

parseToBuildSubdirectory :: FilePath -> Parsec.Parsec String () (Stage, FilePath)
parseToBuildSubdirectory root = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgPath <- Parsec.manyTill Parsec.anyChar
        (Parsec.try $ Parsec.string "/build/")
    return (stage, pkgPath)

-- * Registering

registerPackages :: [Context] -> Action ()
registerPackages ctxs = do
    need =<< mapM pkgRegisteredLibraryFile ctxs

    -- Dynamic RTS library files need symlinks (Rules.Rts.rtsRules).
    forM_ ctxs $ \ ctx -> when (package ctx == rts) $ do
        ways <- interpretInContext ctx (getLibraryWays <> getRtsWays)
        needRtsSymLinks (stage ctx) ways

-- | Register a package and initialise the corresponding package database if
-- need be. Note that we only register packages in 'Stage0' and 'Stage1'.
registerPackageRules :: [(Resource, Int)] -> Stage -> Inplace -> Rules ()
registerPackageRules rs stage iplace = do
    root <- buildRootRules

    -- Initialise the package database.
    root -/- relativePackageDbPath (PackageDbLoc stage iplace) -/- packageDbStamp %> \stamp -> do
        -- This command initialises the package.cache file to avoid a race where
        -- a package gets registered but there's not a package.cache file (which
        -- leads to errors in GHC).
        buildWithResources rs $
            target (Context stage compiler vanilla iplace) (GhcPkg Recache stage) [] []
        writeFileLines stamp []

    -- Special rule for registering system-cxx-std-lib
    root -/- relativePackageDbPath (PackageDbLoc stage iplace) -/- systemCxxStdLibConf %> \file -> do
        copyFile ("mk" -/- "system-cxx-std-lib-1.0.conf") file
        buildWithResources rs $
            target (Context stage compiler vanilla iplace) (GhcPkg Recache stage) [] []

    -- Register a package.
    root -/- relativePackageDbPath (PackageDbLoc stage iplace) -/- "*.conf" %> \conf -> do
        historyDisable

        pkgName <- getPackageNameFromConfFile conf
        let pkg = unsafeFindPackageByName pkgName

        when (pkg == compiler) $ need =<< ghcLibDeps stage iplace

        -- Only used in guard when Stage0 {} but can be GlobalLibs or InTreeLibs
        isBoot <- (pkg `notElem`) <$> stagePackages stage

        let ctx = Context stage pkg vanilla iplace
        case stage of
            Stage0 _ | isBoot -> copyConf  rs ctx conf
            _               ->
              -- See Note [Inplace vs Final package databases]
              case iplace of
                Inplace -> buildConfInplace rs ctx conf
                Final   -> buildConfFinal rs ctx conf

buildConfFinal :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConfFinal rs context@Context {..} _conf = do
    depPkgIds <- cabalDependencies context
    ensureConfigured context
    ways <- interpretInContext context (getLibraryWays <> if package == rts then getRtsWays else mempty)
    stamps <- mapM pkgStampFile [ context { way = w } | w <- Set.toList ways ]
    confs <- mapM (\pkgId -> packageDbPath (PackageDbLoc stage Final) <&> (-/- pkgId <.> "conf")) depPkgIds
    -- Important to need these together to avoid introducing a linearisation. This is not the most critical place
    -- though because needing the stamp file, will cause all dependent object files to be built anyway (even if other packages)
    -- so the .conf file being needed will probably not have to build so much (only stuff which is not use transitively). It's
    -- still better though to need both together to give hadrian the best chance possible to build things in parallel.
    need (stamps ++ confs)

    -- We might need some package-db resource to limit read/write, see packageRules.
    path <- buildPath context

    -- Special package cases (these should ideally be rolled into Cabal).
    when (package == rts) $ do
        jsTarget <- isJsTarget stage

        -- If Cabal knew about "generated-headers", we could read them from the
        -- 'configuredCabal' information, and just "need" them here.
        let common_headers =
              [ path -/- "include/DerivedConstants.h"
              , path -/- "include/ghcautoconf.h"
              , path -/- "include/ghcplatform.h"
              ]
            -- headers only required for the native RTS
            native_headers =
             [ path -/- "include/rts/EventLogConstants.h"
             , path -/- "include/rts/EventTypes.h"
             ]
            headers
              | jsTarget  = common_headers
              | otherwise = common_headers ++ native_headers

        need headers

    -- we need to generate this file for GMP
    when (package == ghcInternal) $ do
        bignum <- interpretInContext context getBignumBackend
        when (bignum == "gmp") $
            need [path -/- "include/ghc-gmp.h"]

    -- Copy and register the package.
    Cabal.copyPackage context
    Cabal.registerPackage rs context

    -- We declare that this rule also produces files matching:
    --   - <root>/stage<N>/lib/<arch>-<os>-ghc-<version>/*libHS<pkgid>*
    --     (for .so files, Cabal's registration mechanism places them there)
    --   - <root>/stage<N>/lib/<arch>-<os>-ghc-<version>/<pkgid>/**
    --     (for interface files, static libs, ghci libs, includes, ...)
    --
    -- so that if any change ends up modifying a library (but not its .conf
    -- file), we still rebuild things that depend on it.
    dyndir <- distDynDir context
    distdir <- distDir context
    pkgid <- pkgUnitId stage package
    files <- liftIO $
      (++) <$> getDirectoryFilesIO "." [dyndir -/- "*libHS"++pkgid++"*"]
           <*> getDirectoryFilesIO "." [distdir -/- "**"]
    produces files

buildConfInplace :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConfInplace rs context@Context {..} _conf = do
    depPkgIds <- cabalDependencies context
    ensureConfigured context
    need =<< mapM (\pkgId -> packageDbPath (PackageDbLoc stage Inplace) <&> (-/- pkgId <.> "conf")) depPkgIds

    path <- buildPath context

    -- Special package cases (these should ideally be rolled into Cabal).
    when (package == rts) $
        -- If Cabal knew about "generated-headers", we could read them from the
        -- 'configuredCabal' information, and just "need" them here.
        need [ path -/- "include/DerivedConstants.h"
             , path -/- "include/ghcautoconf.h"
             , path -/- "include/ghcplatform.h"
             , path -/- "include/rts/EventLogConstants.h"
             , path -/- "include/rts/EventTypes.h"
             ]

    -- we need to generate this file for GMP
    when (package == ghcInternal) $ do
        bignum <- interpretInContext context getBignumBackend
        when (bignum == "gmp") $
            need [path -/- "include/ghc-gmp.h"]

    -- Write an "inplace" package conf which points into the build directories
    -- for finding the build products
    Cabal.writeInplacePkgConf context
    conf <- pkgInplaceConfig context
    buildWithResources rs $
      target context (GhcPkg Update stage) [conf] []


copyConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
copyConf rs context@Context {..} conf = do
    depPkgIds <- fmap stdOutToPkgIds . askWithResources rs $
        target context (GhcPkg Dependencies stage) [pkgName package] []
    need =<< mapM (\pkgId -> packageDbPath (PackageDbLoc stage iplace) <&> (-/- pkgId <.> "conf")) depPkgIds
    -- We should unregister if the file exists since @ghc-pkg@ will complain
    -- about existing package: https://github.com/snowleopard/hadrian/issues/543.
    -- Also, we don't always do the unregistration + registration to avoid
    -- repeated work after a full build.
    -- We do not track 'doesFileExist' since we are going to create the file if
    -- it is currently missing. TODO: Is this the right thing to do?
    -- See https://github.com/snowleopard/hadrian/issues/569.
    unlessM (liftIO $ IO.doesFileExist conf) $ do
        buildWithResources rs $
            target context (GhcPkg Unregister stage) [pkgName package] []
        buildWithResources rs $
            target context (GhcPkg Copy stage) [pkgName package] [conf]
  where
    stdOutToPkgIds :: String -> [String]
    stdOutToPkgIds = drop 1 . concatMap words . lines

getPackageNameFromConfFile :: FilePath -> Action String
getPackageNameFromConfFile conf
    | takeBaseName conf == "rts" = return "rts"
    | otherwise = case parseCabalName (takeBaseName conf) of
        Left err -> error $ "getPackageNameFromConfFile: Couldn't parse " ++
                            takeBaseName conf ++ ": " ++ err
        Right (name, _) -> return name

-- | Parse a cabal-like name
parseCabalName :: String -> Either String (String, Version)
-- Try to parse a name with a hash, but otherwise parse a name without one.
parseCabalName s = bimap show id (Cabal.runParsecParser parser "<parseCabalName>" $ Cabal.fieldLineStreamFromString s)
  where
    parser = Cabal.try nameWithHashParser <|> (extractVersion <$> Cabal.parsec)

    extractVersion :: Cabal.PackageId -> (String, Version)
    extractVersion pkg_id = (Cabal.unPackageName $ Cabal.pkgName pkg_id, Cabal.pkgVersion pkg_id)
    -- Definition similar to 'Parsec PackageIdentifier' from Cabal but extended
    -- with logic for parsing the hash (despite not returning it)
    nameWithHashParser :: Cabal.ParsecParser (String, Version)
    nameWithHashParser = Cabal.PP $ \_ -> do
      xs' <- Parsec.sepBy component (Parsec.char '-')
      case reverse xs' of
        _hash:version_str:xs ->
          case Cabal.simpleParsec @Version version_str of
            Nothing -> fail ("failed to parse a version from " <> version_str)
            Just v  ->
              if not (null xs) && all (\c ->  all (/= '.') c && not (all Char.isDigit c)) xs
              then return $ (intercalate "-" (reverse xs), v)
              else fail "all digits or a dot in a portion of package name"
        _ -> fail "couldn't parse a hash, a version and a name"
      where
        component = CabalCharParsing.munch1 (\c ->  Char.isAlphaNum c || c == '.')



-- | Return extra library targets.
extraTargets :: Context -> Action [FilePath]
extraTargets context
    | package context == rts  = needRtsLibffiTargets (Context.stage context)
    | otherwise               = return []

-- | Given a library 'Package' this action computes all of its targets. Needing
-- all the targets should build the library such that it is ready to be
-- registered into the package database.
-- See 'Rules.packageTargets' for the explanation of the @includeGhciLib@
-- parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context@Context {..} = do
    libFile  <- pkgLibraryFile     context
    ghciLib  <- pkgGhciLibraryFile context
    ghciObjsSupported <- targetSupportsGhciObjects stage
    ghci     <- if ghciObjsSupported && includeGhciLib && not (wayUnit Dynamic way)
                then interpretInContext context $ getContextData buildGhciLib
                else return False
    extra    <- extraTargets context
    return $ [ libFile ]
          ++ [ ghciLib | ghci ]
          ++ extra
