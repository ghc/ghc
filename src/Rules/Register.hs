module Rules.Register (registerPackages) where

import Base
import Context
import GHC
import Settings
import Target
import Utilities

import Distribution.ParseUtils
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Version (Version)

import Hadrian.Expression
import Hadrian.Haskell.Cabal.Parse as Cabal

parseCabalName :: String -> Maybe (String, Version)
parseCabalName = readPToMaybe parse
  where parse = (,) <$> (parsePackageName <* Parse.char '-') <*> parseOptVersion

-- | Build rules for registering packages and initialising package databases
-- by running the @ghc-pkg@ utility.
registerPackages :: [(Resource, Int)] -> Context -> Rules ()
registerPackages rs context@Context {..} = do
  root <- buildRootRules
  root -/- relativePackageDbPath stage %>
    buildStamp rs context

  root -/- relativePackageDbPath stage -/- packageDbStamp %> \stamp ->
    writeFileLines stamp []

  root -/- relativePackageDbPath stage -/- "*.conf" %> \conf -> do
    settings <- libPath context <&> (-/- "settings")
    platformConstants <- libPath context <&> (-/- "platformConstants")
    need [settings, platformConstants]
    let Just pkgName | takeBaseName conf == "rts" = Just "rts"
                     | otherwise = fst <$> parseCabalName (takeBaseName conf)
    let Just pkg = findPackageByName pkgName
    bootLibs <- filter isLibrary <$> stagePackages Stage0
    case stage of
      Stage0 | pkg `notElem` bootLibs -> copyConf rs (context { package = pkg }) conf
      _                               -> buildConf rs (context { package = pkg }) conf


buildConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConf _ context@Context {..} _conf = do
    depPkgIds <- cabalDependencies context

    -- setup-config, triggers `ghc-cabal configure`
    -- everything of a package should depend on that
    -- in the first place.
    setupConfig <- (contextPath context) <&> (-/- "setup-config")
    need [setupConfig]
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds

    ways <- interpretInContext context (getLibraryWays <> if package == rts then getRtsWays else mempty)
    need =<< concatMapM (libraryTargets True) [ context { way = w } | w <- ways ]

    -- might need some package-db resource to limit read/write,
    -- see packageRules
    bldPath <- buildPath context

    -- special package cases (these should ideally be rolled into cabal one way or the other)
    when (package == rts) $
      -- iif cabal new about "generated-headers", we could read them from the configuredCabal
      -- information, and just "need" them here.
      need [ bldPath -/- "DerivedConstants.h"
           , bldPath -/- "ghcautoconf.h"
           , bldPath -/- "ghcplatform.h"
           , bldPath -/- "ghcversion.h"
           , bldPath -/- "ffi.h"
           ]

    when (package == integerGmp) $
      need [bldPath -/- "ghc-gmp.h"]

    -- copy and register the package
    copyPackage context
    registerPackage context

copyConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
copyConf rs context@Context {..} conf = do
    depPkgIds <- fmap stdOutToPkgIds . askWithResources rs $
      target context (GhcPkg Dependencies stage) [pkgName package] []
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds
    buildWithResources rs $
      target context (GhcPkg Clone stage) [pkgName package] [conf]

  where
    stdOutToPkgIds :: String -> [String]
    stdOutToPkgIds = drop 1 . concatMap words . lines

buildStamp :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildStamp rs Context {..} path = do
    buildWithResources rs $
        target (vanillaContext stage ghc) (GhcPkg Init stage) [] [path]
    putSuccess $ "| Successfully initialised " ++ path
