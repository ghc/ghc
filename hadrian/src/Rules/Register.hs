module Rules.Register (configurePackageRules, registerPackageRules) where

import Base
import Context
import Hadrian.BuildPath
import Hadrian.Expression
import Packages
import Settings
import Settings.Default
import Target
import Utilities

import Distribution.ParseUtils
import Distribution.Version (Version)

import qualified Distribution.Compat.ReadP   as Parse
import qualified Hadrian.Haskell.Cabal.Parse as Cabal
import qualified System.Directory            as IO
import qualified Text.Parsec                 as Parsec

-- * Configuring

-- | Configure a package and build its @setup-config@ file.
configurePackageRules :: Rules ()
configurePackageRules = do
    root <- buildRootRules
    root -/- "**/setup-config" %> \path ->
        parsePath (parseSetupConfig root) "<setup config path parser>" path
          >>= configurePackage

parseSetupConfig :: FilePath -> Parsec.Parsec String () (Stage, FilePath)
parseSetupConfig root = do
  _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
  stage <- parseStage
  _ <- Parsec.char '/'
  pkgPath <- Parsec.manyTill Parsec.anyChar
    (Parsec.try $ Parsec.string "/setup-config")
  return (stage, pkgPath)

configurePackage :: (Stage, FilePath) -> Action ()
configurePackage (stage, pkgpath) = do
  pkg <- getPackageByPath pkgpath
  Cabal.configurePackage (Context stage pkg vanilla)

-- * Registering

-- | Register a package and initialise the corresponding package database if
-- need be. Note that we only register packages in 'Stage0' and 'Stage1'.
registerPackageRules :: [(Resource, Int)] -> Stage -> Rules ()
registerPackageRules rs stage = do
    root <- buildRootRules

    -- Initialise the package database.
    root -/- relativePackageDbPath stage -/- packageDbStamp %> \stamp ->
        writeFileLines stamp []

    -- Register a package.
    root -/- relativePackageDbPath stage -/- "*.conf" %> \conf -> do
        let libpath = takeDirectory (takeDirectory conf)
            settings = libpath -/- "settings"
            platformConstants = libpath -/- "platformConstants"

        need [settings, platformConstants]

        pkgName <- getPackageNameFromConfFile conf
        pkg <- getPackageByName pkgName
        isBoot <- (pkg `notElem`) <$> stagePackages Stage0

        let ctx = Context stage pkg vanilla
        case stage of
            Stage0 | isBoot -> copyConf  rs ctx conf
            _               -> buildConf rs ctx conf

buildConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConf _ context@Context {..} _conf = do
    depPkgIds <- cabalDependencies context

    -- Calling 'need' on @setupConfig@, triggers the package configuration.
    setupConfig <- pkgSetupConfigFile context
    need [setupConfig]
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds

    ways <- interpretInContext context (getLibraryWays <> if package == rts then getRtsWays else mempty)
    need =<< concatMapM (libraryTargets True) [ context { way = w } | w <- ways ]

    -- We might need some package-db resource to limit read/write, see packageRules.
    path <- buildPath context

    -- Special package cases (these should ideally be rolled into Cabal).
    when (package == rts) $
        -- If Cabal knew about "generated-headers", we could read them from the
        -- 'configuredCabal' information, and just "need" them here.
        need [ path -/- "DerivedConstants.h"
             , path -/- "ghcautoconf.h"
             , path -/- "ghcplatform.h"
             , path -/- "ghcversion.h"
             , path -/- "ffi.h" ]

    when (package == integerGmp) $ need [path -/- "ghc-gmp.h"]

    -- Copy and register the package.
    Cabal.copyPackage context
    Cabal.registerPackage context

copyConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
copyConf rs context@Context {..} conf = do
    depPkgIds <- fmap stdOutToPkgIds . askWithResources rs $
        target context (GhcPkg Dependencies stage) [pkgName package] []
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds
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
      Nothing -> error $ "getPackageNameFromConfFile: couldn't parse " ++ conf
      Just (name, _) -> return name

parseCabalName :: String -> Maybe (String, Version)
parseCabalName = readPToMaybe parse
  where
    parse = (,) <$> (parsePackageName <* Parse.char '-') <*> parseOptVersion

getPackageByName :: String -> Action Package
getPackageByName n = case findPackageByName n of
  Nothing -> error $ "getPackageByName: couldn't find " ++ n
  Just p  -> return p
