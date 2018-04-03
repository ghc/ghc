{-# OPTIONS_GHC -Wno-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Parse
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Extracting Haskell package metadata stored in Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Parse
  ( PackageData (..), parseCabal, parsePackageData
  , parseCabalPkgId
  , configurePackage, copyPackage, registerPackage
  ) where

import Data.List.Extra
import Development.Shake
import qualified Distribution.ModuleName                       as ModuleName
import qualified Distribution.Package                          as C
import qualified Distribution.PackageDescription               as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Distribution.PackageDescription.Parsec        as C
import qualified Distribution.Simple.Compiler                  as C (packageKeySupported, languageToFlags, extensionsToFlags, compilerInfo)
import qualified Distribution.Simple.GHC                       as GHC
import qualified Distribution.Simple.Program.Db                as Db
import qualified Distribution.Simple                           as Hooks (simpleUserHooks, autoconfUserHooks, defaultMainWithHooksNoReadArgs, compilerFlavor, CompilerFlavor(GHC))
import qualified Distribution.Simple.UserHooks                 as Hooks
import qualified Distribution.Simple.Program.Builtin           as C
import qualified Distribution.Simple.Utils                     as C (findHookedPackageDesc)
import qualified Distribution.Simple.Program.Types             as C (programDefaultArgs, programOverrideArgs)
import qualified Distribution.Simple.Configure                 as C (getPersistBuildConfig)
import qualified Distribution.Simple.Build                     as C (initialBuildSteps)
import qualified Distribution.Types.ComponentRequestedSpec     as C (defaultComponentRequestedSpec)
import qualified Distribution.InstalledPackageInfo             as Installed
import qualified Distribution.Simple.PackageIndex              as PackageIndex
import qualified Distribution.Types.LocalBuildInfo             as C
import qualified Distribution.Text                             as C
import qualified Distribution.Types.MungedPackageId            as C (mungedName)
import qualified Distribution.Verbosity                        as C

import Base
import Builder hiding (Builder)
import Context
import Flavour (args)
import GHC.Packages (rts)
import Hadrian.Expression
import Hadrian.Haskell.Cabal.PackageData
import Hadrian.Haskell.Cabal.Type ( Cabal( Cabal ) )
import Hadrian.Oracles.TextFile
import Hadrian.Target
import Settings
import Oracles.Setting

-- | Parse the Cabal package identifier from the .cabal file at the given
--   filepath.
parseCabalPkgId :: FilePath -> IO String
parseCabalPkgId file = C.display . C.package . C.packageDescription <$> C.readGenericPackageDescription C.silent file


biModules :: C.PackageDescription -> (C.BuildInfo, [ModuleName.ModuleName])
biModules pd = go [ comp | comp@(bi,_) <- (map libBiModules . maybeToList $ C.library pd)
                                         ++ (map exeBiModules $ C.executables pd)
                        , C.buildable bi ]
  where libBiModules lib = (C.libBuildInfo lib, C.explicitLibModules lib)
        exeBiModules exe = (C.buildInfo exe
                           , if isHaskell (C.modulePath exe) -- if "main-is: ..." is not a .hs or .lhs file, do
                                                             -- not inject "Main" into the modules.  This does
                                                             -- not respect "-main-is" ghc-arguments!  See GHC.hs
                                                             -- in Distribution.Simple.GHC from Cabal for the glory
                                                             -- details.
                             then ModuleName.main : C.exeModules exe
                             else C.exeModules exe)
        go [] = error "no buildable component found"
        go [x] = x
        go _  = error "can not handle more than one buildinfo yet!"
        isHaskell fp = takeExtension fp `elem` [".hs", ".lhs"]

-- | Parse the cabal file of the package from the given 'Context'.
--
--   This function reads the cabal file, gets some information about the compiler
--   to be used corresponding to the stage it gets from the 'Context', and finalizes
--   the package description it got from the cabal file with the additional information
--   it got (e.g platform, compiler version conditionals, package flags).
parseCabal :: Context -> Action Cabal
parseCabal context@Context {..} = do
    let (Just file) = pkgCabalFile package

    -- read the package description from the cabal file
    gpd <- liftIO $ C.readGenericPackageDescription C.verbose file

    -- configure the package with the ghc compiler for this stage.
    hcPath <- builderPath (Ghc CompileHs stage)
    (compiler, Just platform, _pgdb) <- liftIO $ GHC.configure C.silent (Just hcPath) Nothing Db.emptyProgramDb


    flagList <- interpret (target context (CabalFlags stage) [] []) =<< args <$> flavour
    let flags = foldr addFlag mempty flagList
          where addFlag :: String -> C.FlagAssignment -> C.FlagAssignment
                addFlag ('-':name) = C.insertFlagAssignment (C.mkFlagName name) False
                addFlag ('+':name) = C.insertFlagAssignment (C.mkFlagName name) True
                addFlag name       = C.insertFlagAssignment (C.mkFlagName name) True

    let (Right (pd,_)) = C.finalizePD flags C.defaultComponentRequestedSpec (const True) platform (C.compilerInfo compiler) [] gpd
    -- depPkgs are all those packages that are needed. These should be found in
    -- the known build packages.  Even if they are not build in this stage.
    let depPkgs = map (findPackageByName' . C.unPackageName . C.depPkgName)
                . flip C.enabledBuildDepends C.defaultComponentRequestedSpec $ pd
          where findPackageByName' p = case findPackageByName p of
                  Just p' -> p'
                  Nothing -> error $ "Failed to find package: " ++ show p
    return $ Cabal (C.unPackageName . C.pkgName . C.package $ pd)
                   (C.display . C.pkgVersion . C.package $ pd)
                   (C.synopsis pd)
                   gpd
                   pd
                   depPkgs

-- | This function runs the equivalent of @cabal configure@ using the Cabal library
--   directly, collecting all the configuration options and flags to be passed to Cabal
--   before invoking it.
--
--   It of course also 'need's package database entries for the dependencies of
--   the package the 'Context' points to.
configurePackage :: Context -> Action ()
configurePackage context@Context {..} = do
    Just (Cabal _ _ _ gpd _pd depPkgs) <- readCabalFile context

    -- Stage packages are those we have in this stage.
    stagePkgs <- stagePackages stage
    -- we'll need those package in our package database.
    need =<< sequence [ pkgConfFile (context { package = pkg }) | pkg <- depPkgs, pkg `elem` stagePkgs ]

    -- figure out what hooks we need.
    hooks <- case C.buildType (C.flattenPackageDescription gpd) of
          C.Configure -> pure Hooks.autoconfUserHooks
          -- time has a "Custom" Setup.hs, but it's actually Configure
          -- plus a "./Setup test" hook. However, Cabal is also
          -- "Custom", but doesn't have a configure script.
          C.Custom ->
              do configureExists <- doesFileExist (replaceFileName (unsafePkgCabalFile package) "configure")
                 if configureExists
                     then pure Hooks.autoconfUserHooks
                     else pure Hooks.simpleUserHooks
          -- not quite right, but good enough for us:
          _ | package == rts ->
              -- don't try to do post conf validation for rts.
              -- this will simply not work, due to the ld-options,
              -- and the Stg.h.
              pure $ Hooks.simpleUserHooks { Hooks.postConf = \_ _ _ _ -> return () }
            | otherwise -> pure Hooks.simpleUserHooks


    case pkgCabalFile package of
      Nothing -> error "No a cabal package!"
      Just _ -> do
        -- compute the flaglist
        flagList <- interpret (target context (CabalFlags stage) [] []) =<< args <$> flavour
        -- compute the cabal conf args
        argList <- interpret (target context (GhcCabal Conf stage) [] []) =<< args <$> flavour
        liftIO $ do
          Hooks.defaultMainWithHooksNoReadArgs hooks gpd (argList ++ ["--flags=" ++ unwords flagList])

-- | Copies a built package (that the 'Context' points to) into a package
--   database (the one for the ghc corresponding to the stage the 'Context'
--   points to).
copyPackage :: Context -> Action ()
copyPackage context@Context {..} = do
  -- original invocation
    Just (Cabal _ _ _ gpd _ _) <- readCabalFile context

    top     <- topDirectory
    ctxPath <- (top -/-) <$> Context.contextPath context
    pkgDbPath <- (top -/-) <$> packageDbPath stage

    let userHooks = Hooks.autoconfUserHooks
        copyHooks = userHooks
        hooks = copyHooks

    liftIO $ Hooks.defaultMainWithHooksNoReadArgs hooks gpd ["copy", "--builddir", ctxPath, "--target-package-db", pkgDbPath]

-- | Registers a built package (the one the 'Context' points to)
--   into the package database.
registerPackage :: Context -> Action ()
registerPackage context@Context {..} = do
    top     <- topDirectory
    ctxPath <- (top -/-) <$> Context.contextPath context
    Just (Cabal _ _ _ gpd _ _) <- readCabalFile context
    let userHooks = Hooks.autoconfUserHooks
        regHooks = userHooks

    liftIO $
      Hooks.defaultMainWithHooksNoReadArgs regHooks gpd ["register", "--builddir", ctxPath]

-- | Parses the 'PackageData' for a package (the one in the 'Context').
parsePackageData :: Context -> Action PackageData
parsePackageData context@Context {..} = do
    -- XXX: This is conceptually wrong!
    --      We should use the gpd, and
    --      the flagAssignment and compiler, hostPlatform, ... information
    --      from the lbi.  And then compute the finaliz PD (flags, satisfiable dependencies, platform, compiler info, deps, gpd.)
    --
    -- let (Right (pd,_)) = C.finalizePackageDescription flags (const True) platform (compilerInfo compiler) [] gpd
    --
    -- However when using the new-build path's this might change.

    Just (Cabal _ _ _ _gpd pd _depPkgs) <- readCabalFile context

    cPath <- Context.contextPath context
    need [cPath -/- "setup-config"]

    lbi <- liftIO $ C.getPersistBuildConfig cPath

    -- XXX: move this into it's own rule for build/autogen/cabal_macros.h, and build/autogen/Path_*.hs
    --      and "need" them here.
    -- create the cabal_macros.h, ...
    -- Note: the `cPath` is ignored. The path that's used is the `buildDir` path from the local build info (lbi).
    pdi <- liftIO $ getHookedBuildInfo (pkgPath package)
    let pd' = C.updatePackageDescription pdi pd
        lbi' = lbi { C.localPkgDescr = pd' }
    liftIO $ C.initialBuildSteps cPath pd' lbi' C.silent

    -- TODO: Get rid of deprecated 'externalPackageDeps' and drop -Wno-deprecations
    -- See: https://github.com/snowleopard/hadrian/issues/548
    let extDeps = C.externalPackageDeps lbi'
        deps    = map (C.display . snd) extDeps
        dep_direct = map (fromMaybe (error "dep_keys failed")
                          . PackageIndex.lookupUnitId (C.installedPkgs lbi')
                          . fst) extDeps
        dep_ipids = map (C.display . Installed.installedUnitId) dep_direct

        Just ghcProg = Db.lookupProgram C.ghcProgram (C.withPrograms lbi')

        dep_pkgs = PackageIndex.topologicalOrder (packageHacks (C.installedPkgs lbi'))
        forDeps f = concatMap f dep_pkgs

        -- copied from Distribution.Simple.PreProcess.ppHsc2Hs
        packageHacks = case Hooks.compilerFlavor (C.compiler lbi') of
          Hooks.GHC | C.pkgName (C.package pd') /= (C.mkPackageName "rts") -> hackRtsPackage
          _   -> id
        -- We don't link in the actual Haskell libraries of our
        -- dependencies, so the -u flags in the ldOptions of the rts
        -- package mean linking fails on OS X (it's ld is a tad
        -- stricter than gnu ld). Thus we remove the ldOptions for
        -- GHC's rts package:
        hackRtsPackage index | null (PackageIndex.allPackages index) = index
        -- ^ do not hack the empty index
        hackRtsPackage index =
          case PackageIndex.lookupPackageName index (C.mkPackageName "rts") of
            [(_,[rts])] ->
              PackageIndex.insert rts{
                Installed.ldOptions = [],
                Installed.libraryDirs = filter (not . ("gcc-lib" `isSuffixOf`)) (Installed.libraryDirs rts)} index
                    -- GHC <= 6.12 had $topdir/gcc-lib in their
                    -- library-dirs for the rts package, which causes
                    -- problems when we try to use the in-tree mingw,
                    -- due to accidentally picking up the incompatible
                    -- libraries there.  So we filter out gcc-lib from
                    -- the RTS's library-dirs here.
            _ -> error "No (or multiple) ghc rts package is registered!!"

      in return $ PackageData
      { dependencies = deps
      , name     = C.unPackageName . C.pkgName . C.package $ pd'
      , version  = C.display . C.pkgVersion . C.package $ pd'
      , componentId = C.localCompatPackageKey lbi'
      , modules  = map C.display . snd . biModules $ pd'
      , otherModules = map C.display . C.otherModules . fst . biModules $ pd'
      , synopsis = C.synopsis pd'
      , description = C.description pd'
      , srcDirs = C.hsSourceDirs . fst . biModules $ pd'
      , deps = deps
      , depIpIds = dep_ipids
      , depNames = map (C.display . C.mungedName . snd) extDeps
      , depCompIds = if C.packageKeySupported (C.compiler lbi')
                     then dep_ipids
                     else deps
      , includeDirs = C.includeDirs . fst . biModules $ pd'
      , includes    = C.includes . fst . biModules $ pd'
      , installIncludes = C.installIncludes . fst . biModules $ pd'
      , extraLibs = C.extraLibs . fst . biModules $ pd'
      , extraLibDirs = C.extraLibDirs . fst . biModules $ pd'
      , asmSrcs = C.asmSources . fst . biModules $ pd'
      , cSrcs   = C.cSources . fst . biModules $ pd'
      , cmmSrcs = C.cmmSources . fst . biModules $ pd'
      , dataFiles = C.dataFiles pd'
      , hcOpts    =    C.programDefaultArgs ghcProg
                    ++ (C.hcOptions Hooks.GHC . fst . biModules $ pd')
                    ++ C.languageToFlags (C.compiler lbi') (C.defaultLanguage . fst . biModules $ pd')
                    ++ C.extensionsToFlags (C.compiler lbi') (C.usedExtensions . fst . biModules $ pd')
                    ++ C.programOverrideArgs ghcProg
      , asmOpts   = C.asmOptions . fst . biModules $ pd'
      , ccOpts    = C.ccOptions . fst . biModules $ pd'
      , cmmOpts   = C.cmmOptions . fst . biModules $ pd'
      , cppOpts   = C.cppOptions . fst . biModules $ pd'
      , ldOpts    = C.ldOptions . fst . biModules $ pd'
      , depIncludeDirs = forDeps Installed.includeDirs
      , depCcOpts = forDeps Installed.ccOptions
      , depLdOpts = forDeps Installed.ldOptions
      , buildGhciLib = C.withGHCiLib lbi'
      }

getHookedBuildInfo :: FilePath -> IO C.HookedBuildInfo
getHookedBuildInfo baseDir = do
  -- TODO: We should probably better generate this in the
  --       build dir, rather then in the base dir? However
  --       `configure` is run in the baseDir.

  maybe_infoFile <- C.findHookedPackageDesc baseDir
  case maybe_infoFile of
    Nothing       -> return C.emptyHookedBuildInfo
    Just infoFile -> C.readHookedBuildInfo C.silent infoFile
