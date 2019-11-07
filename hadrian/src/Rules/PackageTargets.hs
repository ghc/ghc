module Rules.PackageTargets
  ( simplePackageTargets
  , needPackage, needPackages
  , packageDependencies
  , allLibraryTargets, allInstalledLibraryTargets
  ) where

import Base
import Context
import Expression hiding (package)
import Packages
import Rules.Rts
import Settings

import Hadrian.Haskell.Cabal (pkgDependencies)
import Hadrian.Haskell.Cabal.Type (buildGhciLib)

-- | Simple targets for library and executable targets.
--
--   @stage<N>:<name>@ will build library or program @name@ with
--   the stage N compiler, putting the result under
--   @<build root>/stage<N>/lib@.
--
--   For executables, the target just maps to the suitable
--   executable under @<build root>/stage<N>/bin@.
--   For libraries, the target maps to the @.conf@ entry in the
--   package database + the library files (static, shared, ghci).
simplePackageTargets :: Rules ()
simplePackageTargets = sequence_ allPkgTargets

  where allPkgTargets =
          [ simpleTarget s p
          | s <- [minBound..maxBound]
          , p <- knownPackages
          ]

simpleTarget :: Stage -> Package -> Rules ()
simpleTarget stage target = do
  let tgt = intercalate ":" [stagestr, pkgname]
  tgt ~> needPackage stage target

  where stagestr = stageString stage
        pkgname = pkgName target

-- | Given a library 'Context' this action computes all of its targets. Needing
-- all the targets should build the library such that it is ready to be
-- registered into the package database.
-- See 'packageTargets' for the explanation of the @includeGhciLib@ parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context@Context {..} = do
    libFile  <- pkgLibraryFile     context
    ghciLib  <- pkgGhciLibraryFile context
    ghci     <- if includeGhciLib && not (wayUnit Dynamic way)
                then interpretInContext context $ getContextData buildGhciLib
                else return False
    extra    <- extraTargets context
    return $ [ libFile ]
          ++ [ ghciLib | ghci ]
          ++ extra

installedLibraryTargets :: Bool -> Context -> Action [FilePath]
installedLibraryTargets includeGhciLib context@Context {..} = do
  libFile <- pkgRegisteredLibraryFile context
  ghciLib <- pkgRegisteredGhciLibraryFile context
  ghci    <- if includeGhciLib && not (wayUnit Dynamic way)
             then interpretInContext context $ getContextData buildGhciLib
             else return False
  return $ [ libFile ] ++ [ ghciLib | ghci ]

-- | Return extra library targets (non-empty only for the RTS library).
extraTargets :: Context -> Action [FilePath]
extraTargets context
    | package context == rts  = needRtsLibffiTargets (Context.stage context)
    | otherwise               = return []

-- | Given a library 'Package' and a 'Stage' this action returns
--   all the static, shared and ghci library files that we
--   should build.
--
--   Needing all the targets should build the library such
--   that it is ready to be registered into the package
--   database.
--
--   See 'packageTargets' for the explanation of the
--   @includeGhciLib@ parameter.
allLibraryTargets :: Bool -> Stage -> Package -> Action [FilePath]
allLibraryTargets includeGhciLib s p = do
    ways <- interpretInContext (vanillaContext s p) $
      getLibraryWays <>
      (if p == rts then getRtsWays else mempty)
    ts <- concatMapM (libraryTargets includeGhciLib) [ Context s p w | w <- ways ]
    return ts

allInstalledLibraryTargets :: Bool -> Stage -> Package -> Action [FilePath]
allInstalledLibraryTargets includeGhciLib s p = do
  ways <- interpretInContext (vanillaContext s p) $
    getLibraryWays <> (if p == rts then getRtsWays else mempty)
  ts <- concatMapM (installedLibraryTargets includeGhciLib)
                   [ Context s p w | w <- ways ]
  return ts

getInstalledTargetPaths :: Stage -> Package -> Action [FilePath]
getInstalledTargetPaths s p
  | isLibrary p = (\fp -> [fp]) <$> getLibraryConfPath s p
  | otherwise = (\fp -> [fp]) <$> getProgramPath s p

  where
    getProgramPath :: Stage -> Package -> Action FilePath
    getProgramPath Stage0 p =
      error ("Cannot build a stage 0 executable target: " ++
             "it is the boot compiler's toolchain - " ++ pkgName p)
    getProgramPath stage pkg = programPath (vanillaContext (pred stage) pkg)

    getLibraryConfPath :: Stage -> Package -> Action FilePath
    getLibraryConfPath stage pkg = pkgConfFile (vanillaContext stage pkg)

needPackage :: Stage -> Package -> Action ()
needPackage s p = getInstalledTargetPaths s p >>= need

needPackages :: Stage -> [Package] -> Action ()
needPackages s pkgs = concatMapM (getInstalledTargetPaths s) pkgs >>= need

packageDependencies :: Stage -> Package -> Action [Package]
packageDependencies s p = filter (/=p) <$> go [p]

  where go pkgs = do
          deps <- concatMapM step pkgs
          let newPkgs = nubOrd $ sort (deps ++ pkgs)
          if pkgs == newPkgs then return pkgs else go newPkgs
        step pkg = do
          deps <- pkgDependencies pkg
          active <- sort <$> stagePackages s
          return $ filter (\p' -> pkgName p' `elem` deps) active
