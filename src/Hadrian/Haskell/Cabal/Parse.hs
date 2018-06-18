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
  ( PackageData (..), parseCabal, parsePackageData, parseCabalPkgId
  , configurePackage, copyPackage, registerPackage
  ) where

import Data.List.Extra
import Development.Shake
import qualified Distribution.ModuleName                       as C
import qualified Distribution.Package                          as C
import qualified Distribution.PackageDescription               as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Distribution.PackageDescription.Parsec        as C
import qualified Distribution.Simple.Compiler                  as C
import qualified Distribution.Simple.GHC                       as C
import qualified Distribution.Simple.Program.Db                as C
import qualified Distribution.Simple                           as C
import qualified Distribution.Simple.Program.Builtin           as C
import qualified Distribution.Simple.Utils                     as C
import qualified Distribution.Simple.Program.Types             as C
import qualified Distribution.Simple.Configure                 as C (getPersistBuildConfig)
import qualified Distribution.Simple.Build                     as C
import qualified Distribution.Types.ComponentRequestedSpec     as C
import qualified Distribution.InstalledPackageInfo             as Installed
import qualified Distribution.Simple.PackageIndex              as C
import qualified Distribution.Types.LocalBuildInfo             as C
import qualified Distribution.Text                             as C
import qualified Distribution.Types.MungedPackageId            as C
import qualified Distribution.Verbosity                        as C

import Base
import Builder
import Context
import Flavour
import GHC.Packages
import Hadrian.Expression
import Hadrian.Haskell.Cabal.PackageData
import Hadrian.Haskell.Cabal.Type
import Hadrian.Oracles.TextFile
import Hadrian.Target
import Settings

-- | Parse the Cabal package identifier from a @.cabal@ file.
parseCabalPkgId :: FilePath -> IO String
parseCabalPkgId file = C.display . C.package . C.packageDescription <$> C.readGenericPackageDescription C.silent file

biModules :: C.PackageDescription -> (C.BuildInfo, [C.ModuleName], Maybe (C.ModuleName, String))
biModules pd = go [ comp | comp@(bi,_,_) <-
                             (map libBiModules . maybeToList $ C.library pd) ++
                             (map exeBiModules               $ C.executables pd)
                         , C.buildable bi ]
  where
    libBiModules lib = (C.libBuildInfo lib, C.explicitLibModules lib, Nothing)
    exeBiModules exe = (C.buildInfo exe,
                       -- If "main-is: ..." is not a .hs or .lhs file, do not
                       -- inject "Main" into the modules.  This does not respect
                       -- "-main-is" ghc-arguments! See Cabal's
                       -- Distribution.Simple.GHC for the glory details.
                       if takeExtension (C.modulePath exe) `elem` [".hs", ".lhs"]
                           then C.main : C.exeModules exe
                                -- The module `Main` still need to be kept in `modules` of PD.
                           else C.exeModules exe,
                       Just (C.main, C.modulePath exe))
    go []  = error "No buildable component found."
    go [x] = x
    go _   = error "Cannot handle more than one buildinfo yet."

-- TODO: Add proper error handling for partiality due to Nothing/Left cases.
-- | Parse the Cabal file of the 'Package' from a given 'Context'. This function
-- reads the Cabal file, gets some information about the compiler to be used
-- corresponding to the 'Stage' it gets from the 'Context', and finalises the
-- package description it got from the Cabal file with additional information
-- such as platform, compiler version conditionals, and package flags.
parseCabal :: Context -> Action Cabal
parseCabal context@Context {..} = do
    let file = unsafePkgCabalFile package

    -- Read the package description from the Cabal file
    gpd <- liftIO $ C.readGenericPackageDescription C.verbose file

    -- Configure the package with the GHC for this stage
    hcPath <- builderPath (Ghc CompileHs stage)
    (compiler, Just platform, _pgdb) <- liftIO $
        C.configure C.silent (Just hcPath) Nothing C.emptyProgramDb

    flagList <- interpret (target context (CabalFlags stage) [] []) =<< args <$> flavour
    let flags = foldr addFlag mempty flagList
          where
            addFlag :: String -> C.FlagAssignment -> C.FlagAssignment
            addFlag ('-':name) = C.insertFlagAssignment (C.mkFlagName name) False
            addFlag ('+':name) = C.insertFlagAssignment (C.mkFlagName name) True
            addFlag name       = C.insertFlagAssignment (C.mkFlagName name) True

    let (Right (pd,_)) = C.finalizePD flags C.defaultComponentRequestedSpec
                         (const True) platform (C.compilerInfo compiler) [] gpd
    -- depPkgs are all those packages that are needed. These should be found in
    -- the known build packages even if they are not build in this stage.
    let depPkgs = map (findPackageByName' . C.unPackageName . C.depPkgName)
                $ flip C.enabledBuildDepends C.defaultComponentRequestedSpec pd
          where
            findPackageByName' p = fromMaybe (error msg) (findPackageByName p)
              where
                msg = "Failed to find package " ++ quote (show p)
    return $ Cabal (C.unPackageName . C.pkgName . C.package $ pd)
                   (C.display . C.pkgVersion . C.package $ pd)
                   (C.synopsis pd) gpd pd depPkgs

-- | This function runs the equivalent of @cabal configure@ using the Cabal
-- library directly, collecting all the configuration options and flags to be
-- passed to Cabal before invoking it. It 'need's package database entries for
-- the dependencies of the package the 'Context' points to.
configurePackage :: Context -> Action ()
configurePackage context@Context {..} = do
    putLoud $ "| Configure package " ++ quote (pkgName package)

    Cabal _ _ _ gpd _pd depPkgs <- unsafeReadCabalFile context

    -- Stage packages are those we have in this stage.
    stagePkgs <- stagePackages stage
    -- We'll need those packages in our package database.
    deps <- sequence [ pkgConfFile (context { package = pkg })
                     | pkg <- depPkgs, pkg `elem` stagePkgs ]
    need deps

    -- Figure out what hooks we need.
    hooks <- case C.buildType (C.flattenPackageDescription gpd) of
        C.Configure -> pure C.autoconfUserHooks
        -- time has a "Custom" Setup.hs, but it's actually Configure
        -- plus a "./Setup test" hook. However, Cabal is also
        -- "Custom", but doesn't have a configure script.
        C.Custom -> do
            configureExists <- doesFileExist $
                replaceFileName (unsafePkgCabalFile package) "configure"
            pure $ if configureExists then C.autoconfUserHooks else C.simpleUserHooks
        -- Not quite right, but good enough for us:
        _ | package == rts ->
            -- Don't try to do post conf validation for rts. This will simply
            -- not work, due to the ld-options and the Stg.h.
            pure $ C.simpleUserHooks { C.postConf = \_ _ _ _ -> return () }
          | otherwise -> pure C.simpleUserHooks

    -- Compute the list of flags
    -- Compute the Cabal configurartion arguments
    flavourArgs <- args <$> flavour
    flagList    <- interpret (target context (CabalFlags    stage) [] []) flavourArgs
    argList     <- interpret (target context (GhcCabal Conf stage) [] []) flavourArgs
    verbosity   <- getVerbosity
    let v = if verbosity >= Loud then "-v3" else "-v0"
    liftIO $ C.defaultMainWithHooksNoReadArgs hooks gpd
        (argList ++ ["--flags=" ++ unwords flagList, v])

-- | Copy the 'Package' of a given 'Context' into the package database
-- corresponding to the 'Stage' of the 'Context'.
copyPackage :: Context -> Action ()
copyPackage context@Context {..} = do
    putLoud $ "| Copy package " ++ quote (pkgName package)
    Cabal _ _ _ gpd _ _ <- unsafeReadCabalFile context
    ctxPath   <- Context.contextPath context
    pkgDbPath <- packageDbPath stage
    verbosity <- getVerbosity
    let v = if verbosity >= Loud then "-v3" else "-v0"
    liftIO $ C.defaultMainWithHooksNoReadArgs C.autoconfUserHooks gpd
        [ "copy", "--builddir", ctxPath, "--target-package-db", pkgDbPath, v ]

-- | Register the 'Package' of a given 'Context' into the package database.
registerPackage :: Context -> Action ()
registerPackage context@Context {..} = do
    putLoud $ "| Register package " ++ quote (pkgName package)
    ctxPath <- Context.contextPath context
    Cabal _ _ _ gpd _ _ <- unsafeReadCabalFile context
    verbosity <- getVerbosity
    let v = if verbosity >= Loud then "-v3" else "-v0"
    liftIO $ C.defaultMainWithHooksNoReadArgs C.autoconfUserHooks gpd
        [ "register", "--builddir", ctxPath, v ]

-- | Parse the 'PackageData' of the 'Package' of a given 'Context'.
parsePackageData :: Context -> Action PackageData
parsePackageData context@Context {..} = do
    -- TODO: This is conceptually wrong!
    -- We should use the gpd, the flagAssignment and compiler, hostPlatform, and
    -- other information from the lbi. And then compute the finalised PD (flags,
    -- satisfiable dependencies, platform, compiler info, deps, gpd).
    --
    -- let (Right (pd,_)) = C.finalizePackageDescription flags (const True) platform (compilerInfo compiler) [] gpd
    --
    -- However when using the new-build path's this might change.
    Cabal _ _ _ _gpd pd _depPkgs <- unsafeReadCabalFile context

    cPath <- Context.contextPath context
    need [cPath -/- "setup-config"]

    lbi <- liftIO $ C.getPersistBuildConfig cPath

    -- TODO: Move this into its own rule for "build/autogen/cabal_macros.h", and
    -- "build/autogen/Path_*.hs" and 'need' them here.
    -- create the cabal_macros.h, ...
    -- Note: the `cPath` is ignored. The path that's used is the 'buildDir' path
    -- from the local build info (lbi).
    pdi <- liftIO $ getHookedBuildInfo (pkgPath package)
    let pd'  = C.updatePackageDescription pdi pd
        lbi' = lbi { C.localPkgDescr = pd' }
    liftIO $ C.initialBuildSteps cPath pd' lbi' C.silent

    -- TODO: Get rid of deprecated 'externalPackageDeps' and drop -Wno-deprecations
    -- See: https://github.com/snowleopard/hadrian/issues/548
    let extDeps      = C.externalPackageDeps lbi'
        deps         = map (C.display . snd) extDeps
        dep_direct   = map (fromMaybe (error "parsePackageData: dep_keys failed")
                          . C.lookupUnitId (C.installedPkgs lbi') . fst) extDeps
        dep_ipids    = map (C.display . Installed.installedUnitId) dep_direct
        Just ghcProg = C.lookupProgram C.ghcProgram (C.withPrograms lbi')
        dep_pkgs     = C.topologicalOrder (packageHacks (C.installedPkgs lbi'))
        forDeps f    = concatMap f dep_pkgs

        -- Copied from Distribution.Simple.PreProcess.ppHsc2Hs
        packageHacks = case C.compilerFlavor (C.compiler lbi') of
            C.GHC | C.pkgName (C.package pd') /= (C.mkPackageName "rts") -> hackRtsPackage
            _   -> id

        -- TODO: Get rid of this hack.
        -- We don't link in the actual Haskell libraries of our dependencies, so
        -- the "-u" flags in @ldOptions@ of the @rts@ package mean linking fails
        -- on OS X (its @ld@ is a tad stricter than GNU @ld@). Thus we remove
        -- @ldOptions@ for the @rts@ package. With one exception (see below).
        hackRtsPackage index | null (C.allPackages index) = index
        -- ^ do not hack the empty index
        hackRtsPackage index = case C.lookupPackageName index (C.mkPackageName "rts") of
            [(_,[rts])] -> C.insert rts {
                Installed.ldOptions   = [],
                Installed.libraryDirs = filter (not . ("gcc-lib" `isSuffixOf`))
                                               (Installed.libraryDirs rts)} index
            -- GHC <= 6.12 had $topdir/gcc-lib in their library-dirs for the rts
            -- package, which causes problems when we try to use the in-tree
            -- mingw, due to accidentally picking up the incompatible libraries
            -- there. So we filter out gcc-lib from the RTS's library-dirs here.
            _ -> error "No (or multiple) GHC rts package is registered!"

        (buildInfo, modules, mainIs) = biModules pd'

      in return $ PackageData
          { dependencies    = deps
          , name            = C.unPackageName . C.pkgName    . C.package $ pd'
          , version         = C.display       . C.pkgVersion . C.package $ pd'
          , componentId     = C.localCompatPackageKey lbi'
          , mainIs          = case mainIs of
                                   Just (mod, filepath) -> Just (C.display mod, filepath)
                                   Nothing              -> Nothing
          , modules         = map C.display $ modules
          , otherModules    = map C.display . C.otherModules $ buildInfo
          , synopsis        = C.synopsis    pd'
          , description     = C.description pd'
          , srcDirs         = C.hsSourceDirs buildInfo
          , deps            = deps
          , depIpIds        = dep_ipids
          , depNames        = map (C.display . C.mungedName . snd) extDeps
          , depCompIds      = if C.packageKeySupported (C.compiler lbi')
                              then dep_ipids
                              else deps
          , includeDirs     = C.includeDirs     buildInfo
          , includes        = C.includes        buildInfo
          , installIncludes = C.installIncludes buildInfo
          , extraLibs       = C.extraLibs       buildInfo
          , extraLibDirs    = C.extraLibDirs    buildInfo
          , asmSrcs         = C.asmSources      buildInfo
          , cSrcs           = C.cSources        buildInfo
          , cmmSrcs         = C.cmmSources      buildInfo
          , dataFiles       = C.dataFiles pd'
          , hcOpts          = C.programDefaultArgs ghcProg
              ++ C.hcOptions C.GHC buildInfo
              ++ C.languageToFlags   (C.compiler lbi') (C.defaultLanguage buildInfo)
              ++ C.extensionsToFlags (C.compiler lbi') (C.usedExtensions  buildInfo)
              ++ C.programOverrideArgs ghcProg
          , asmOpts         = C.asmOptions buildInfo
          , ccOpts          = C.ccOptions  buildInfo
          , cmmOpts         = C.cmmOptions buildInfo
          , cppOpts         = C.cppOptions buildInfo
          , ldOpts          = C.ldOptions  buildInfo
          , depIncludeDirs  = forDeps Installed.includeDirs
          , depCcOpts       = forDeps Installed.ccOptions
          , depLdOpts       = forDeps Installed.ldOptions
          , buildGhciLib    = C.withGHCiLib lbi' }

getHookedBuildInfo :: FilePath -> IO C.HookedBuildInfo
getHookedBuildInfo baseDir = do
    -- TODO: We should probably better generate this in the build dir, rather
    -- than in the base dir? However, @configure@ is run in the baseDir.
    maybeInfoFile <- C.findHookedPackageDesc baseDir
    case maybeInfoFile of
        Nothing       -> return C.emptyHookedBuildInfo
        Just infoFile -> C.readHookedBuildInfo C.silent infoFile
