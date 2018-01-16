{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with registering and unregistering packages. There are a
-- couple ways it can do this, one is to do it directly. Another is to generate
-- a script that can be run later to do it. The idea here being that the user
-- is shielded from the details of what command to use for package registration
-- for a particular compiler. In practice this aspect was not especially
-- popular so we also provide a way to simply generate the package registration
-- file which then must be manually passed to @ghc-pkg@. It is possible to
-- generate registration information for where the package is to be installed,
-- or alternatively to register the package in place in the build tree. The
-- latter is occasionally handy, and will become more important when we try to
-- build multi-package systems.
--
-- This module does not delegate anything to the per-compiler modules but just
-- mixes it all in in this module, which is rather unsatisfactory. The script
-- generation and the unregister feature are not well used or tested.

module Distribution.Simple.Register (
    register,
    unregister,

    internalPackageDBPath,

    initPackageDB,
    doesPackageDBExist,
    createPackageDB,
    deletePackageDB,

    abiHash,
    invokeHcPkg,
    registerPackage,
    HcPkg.RegisterOptions(..),
    HcPkg.defaultRegisterOptions,
    generateRegistrationInfo,
    inplaceInstalledPackageInfo,
    absoluteInstalledPackageInfo,
    generalInstalledPackageInfo,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.TargetInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.BuildTarget

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import qualified Distribution.Simple.PackageIndex as Index

import Distribution.Backpack.DescribeUnitId
import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Simple.Program.Script
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Package
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Simple.Utils
import Distribution.Utils.MapAccum
import Distribution.System
import Distribution.Text
import Distribution.Types.ComponentName
import Distribution.Verbosity as Verbosity
import Distribution.Version
import Distribution.Compat.Graph (IsNode(nodeKey))

import System.FilePath ((</>), (<.>), isAbsolute)
import System.Directory

import Data.List (partition)
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

-- -----------------------------------------------------------------------------
-- Registration

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg_descr lbi0 flags =
   -- Duncan originally asked for us to not register/install files
   -- when there was no public library.  But with per-component
   -- configure, we legitimately need to install internal libraries
   -- so that we can get them.  So just unconditionally install.
   doRegister
 where
  doRegister = do
    targets <- readTargetInfos verbosity pkg_descr lbi0 (regArgs flags)

    -- It's important to register in build order, because ghc-pkg
    -- will complain if a dependency is not registered.
    let componentsToRegister
            = neededTargetsInBuildOrder' pkg_descr lbi0 (map nodeKey targets)

    (_, ipi_mbs) <-
        mapAccumM `flip` installedPkgs lbi0 `flip` componentsToRegister $ \index tgt ->
            case targetComponent tgt of
                CLib lib -> do
                    let clbi = targetCLBI tgt
                        lbi = lbi0 { installedPkgs = index }
                    ipi <- generateOne pkg_descr lib lbi clbi flags
                    return (Index.insert ipi index, Just ipi)
                _   -> return (index, Nothing)

    registerAll pkg_descr lbi0 flags (catMaybes ipi_mbs)
   where
    verbosity = fromFlag (regVerbosity flags)

generateOne :: PackageDescription -> Library -> LocalBuildInfo -> ComponentLocalBuildInfo
            -> RegisterFlags
            -> IO InstalledPackageInfo
generateOne pkg lib lbi clbi regFlags
  = do
    absPackageDBs    <- absolutePackageDBPaths packageDbs
    installedPkgInfo <- generateRegistrationInfo
                           verbosity pkg lib lbi clbi inplace reloc distPref
                           (registrationPackageDB absPackageDBs)
    info verbosity (IPI.showInstalledPackageInfo installedPkgInfo)
    return installedPkgInfo
  where
    inplace   = fromFlag (regInPlace regFlags)
    reloc     = relocatable lbi
    -- FIXME: there's really no guarantee this will work.
    -- registering into a totally different db stack can
    -- fail if dependencies cannot be satisfied.
    packageDbs = nub $ withPackageDB lbi
                    ++ maybeToList (flagToMaybe  (regPackageDB regFlags))
    distPref  = fromFlag (regDistPref regFlags)
    verbosity = fromFlag (regVerbosity regFlags)

registerAll :: PackageDescription -> LocalBuildInfo -> RegisterFlags
            -> [InstalledPackageInfo]
            -> IO ()
registerAll pkg lbi regFlags ipis
  = do
    when (fromFlag (regPrintId regFlags)) $ do
      for_ ipis $ \installedPkgInfo ->
        -- Only print the public library's IPI
        when (packageId installedPkgInfo == packageId pkg
              && IPI.sourceLibName installedPkgInfo == Nothing) $
          putStrLn (display (IPI.installedUnitId installedPkgInfo))

     -- Three different modes:
    case () of
     _ | modeGenerateRegFile   -> writeRegistrationFileOrDirectory
       | modeGenerateRegScript -> writeRegisterScript
       | otherwise             -> do
           for_ ipis $ \ipi -> do
               setupMessage' verbosity "Registering" (packageId pkg)
                 (libraryComponentName (IPI.sourceLibName ipi))
                 (Just (IPI.instantiatedWith ipi))
               registerPackage verbosity (compiler lbi) (withPrograms lbi)
                               packageDbs ipi HcPkg.defaultRegisterOptions

  where
    modeGenerateRegFile = isJust (flagToMaybe (regGenPkgConf regFlags))
    regFile             = fromMaybe (display (packageId pkg) <.> "conf")
                                    (fromFlag (regGenPkgConf regFlags))

    modeGenerateRegScript = fromFlag (regGenScript regFlags)

    -- FIXME: there's really no guarantee this will work.
    -- registering into a totally different db stack can
    -- fail if dependencies cannot be satisfied.
    packageDbs = nub $ withPackageDB lbi
                    ++ maybeToList (flagToMaybe  (regPackageDB regFlags))
    verbosity = fromFlag (regVerbosity regFlags)

    writeRegistrationFileOrDirectory = do
      -- Handles overwriting both directory and file
      deletePackageDB regFile
      case ipis of
        [installedPkgInfo] -> do
          info verbosity ("Creating package registration file: " ++ regFile)
          writeUTF8File regFile (IPI.showInstalledPackageInfo installedPkgInfo)
        _ -> do
          info verbosity ("Creating package registration directory: " ++ regFile)
          createDirectory regFile
          let num_ipis = length ipis
              lpad m xs = replicate (m - length ys) '0' ++ ys
                  where ys = take m xs
              number i = lpad (length (show num_ipis)) (show i)
          for_ (zip ([1..] :: [Int]) ipis) $ \(i, installedPkgInfo) ->
            writeUTF8File (regFile </> (number i ++ "-" ++ display (IPI.installedUnitId installedPkgInfo)))
                          (IPI.showInstalledPackageInfo installedPkgInfo)

    writeRegisterScript =
      case compilerFlavor (compiler lbi) of
        JHC -> notice verbosity "Registration scripts not needed for jhc"
        UHC -> notice verbosity "Registration scripts not needed for uhc"
        _   -> withHcPkg verbosity
               "Registration scripts are not implemented for this compiler"
               (compiler lbi) (withPrograms lbi)
               (writeHcPkgRegisterScript verbosity ipis packageDbs)


generateRegistrationInfo :: Verbosity
                         -> PackageDescription
                         -> Library
                         -> LocalBuildInfo
                         -> ComponentLocalBuildInfo
                         -> Bool
                         -> Bool
                         -> FilePath
                         -> PackageDB
                         -> IO InstalledPackageInfo
generateRegistrationInfo verbosity pkg lib lbi clbi inplace reloc distPref packageDb = do
  --TODO: eliminate pwd!
  pwd <- getCurrentDirectory

  installedPkgInfo <-
    if inplace
      -- NB: With an inplace installation, the user may run './Setup
      -- build' to update the library files, without reregistering.
      -- In this case, it is critical that the ABI hash not flip.
      then return (inplaceInstalledPackageInfo pwd distPref
                     pkg (mkAbiHash "inplace") lib lbi clbi)
    else do
        abi_hash <- abiHash verbosity pkg distPref lbi lib clbi
        if reloc
          then relocRegistrationInfo verbosity
                         pkg lib lbi clbi abi_hash packageDb
          else return (absoluteInstalledPackageInfo
                         pkg abi_hash lib lbi clbi)


  return installedPkgInfo

-- | Compute the 'AbiHash' of a library that we built inplace.
abiHash :: Verbosity
        -> PackageDescription
        -> FilePath
        -> LocalBuildInfo
        -> Library
        -> ComponentLocalBuildInfo
        -> IO AbiHash
abiHash verbosity pkg distPref lbi lib clbi =
    case compilerFlavor comp of
     GHC | compilerVersion comp >= mkVersion [6,11] -> do
            fmap mkAbiHash $ GHC.libAbiHash verbosity pkg lbi' lib clbi
     GHCJS -> do
            fmap mkAbiHash $ GHCJS.libAbiHash verbosity pkg lbi' lib clbi
     _ -> return (mkAbiHash "")
  where
    comp = compiler lbi
    lbi' = lbi {
              withPackageDB = withPackageDB lbi
                  ++ [SpecificPackageDB (internalPackageDBPath lbi distPref)]
           }

relocRegistrationInfo :: Verbosity
                      -> PackageDescription
                      -> Library
                      -> LocalBuildInfo
                      -> ComponentLocalBuildInfo
                      -> AbiHash
                      -> PackageDB
                      -> IO InstalledPackageInfo
relocRegistrationInfo verbosity pkg lib lbi clbi abi_hash packageDb =
  case (compilerFlavor (compiler lbi)) of
    GHC -> do fs <- GHC.pkgRoot verbosity lbi packageDb
              return (relocatableInstalledPackageInfo
                        pkg abi_hash lib lbi clbi fs)
    _   -> die' verbosity
              "Distribution.Simple.Register.relocRegistrationInfo: \
               \not implemented for this compiler"

initPackageDB :: Verbosity -> Compiler -> ProgramDb -> FilePath -> IO ()
initPackageDB verbosity comp progdb dbPath =
    createPackageDB verbosity comp progdb False dbPath

-- | Create an empty package DB at the specified location.
createPackageDB :: Verbosity -> Compiler -> ProgramDb -> Bool
                -> FilePath -> IO ()
createPackageDB verbosity comp progdb preferCompat dbPath =
    case compilerFlavor comp of
      GHC   -> HcPkg.init (GHC.hcPkgInfo   progdb) verbosity preferCompat dbPath
      GHCJS -> HcPkg.init (GHCJS.hcPkgInfo progdb) verbosity False dbPath
      LHC   -> HcPkg.init (LHC.hcPkgInfo   progdb) verbosity False dbPath
      UHC   -> return ()
      HaskellSuite _ -> HaskellSuite.initPackageDB verbosity progdb dbPath
      _              -> die' verbosity $
                              "Distribution.Simple.Register.createPackageDB: "
                           ++ "not implemented for this compiler"

doesPackageDBExist :: FilePath -> NoCallStackIO Bool
doesPackageDBExist dbPath = do
    -- currently one impl for all compiler flavours, but could change if needed
    dir_exists <- doesDirectoryExist dbPath
    if dir_exists
        then return True
        else doesFileExist dbPath

deletePackageDB :: FilePath -> NoCallStackIO ()
deletePackageDB dbPath = do
    -- currently one impl for all compiler flavours, but could change if needed
    dir_exists <- doesDirectoryExist dbPath
    if dir_exists
        then removeDirectoryRecursive dbPath
        else do file_exists <- doesFileExist dbPath
                when file_exists $ removeFile dbPath

-- | Run @hc-pkg@ using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invokeHcPkg :: Verbosity -> Compiler -> ProgramDb -> PackageDBStack
                -> [String] -> IO ()
invokeHcPkg verbosity comp progdb dbStack extraArgs =
  withHcPkg verbosity "invokeHcPkg" comp progdb
    (\hpi -> HcPkg.invoke hpi verbosity dbStack extraArgs)

withHcPkg :: Verbosity -> String -> Compiler -> ProgramDb
          -> (HcPkg.HcPkgInfo -> IO a) -> IO a
withHcPkg verbosity name comp progdb f =
  case compilerFlavor comp of
    GHC   -> f (GHC.hcPkgInfo progdb)
    GHCJS -> f (GHCJS.hcPkgInfo progdb)
    LHC   -> f (LHC.hcPkgInfo progdb)
    _     -> die' verbosity ("Distribution.Simple.Register." ++ name ++ ":\
                  \not implemented for this compiler")

registerPackage :: Verbosity
                -> Compiler
                -> ProgramDb
                -> PackageDBStack
                -> InstalledPackageInfo
                -> HcPkg.RegisterOptions
                -> IO ()
registerPackage verbosity comp progdb packageDbs installedPkgInfo registerOptions =
  case compilerFlavor comp of
    GHC   -> GHC.registerPackage   verbosity progdb packageDbs installedPkgInfo registerOptions
    GHCJS -> GHCJS.registerPackage verbosity progdb packageDbs installedPkgInfo registerOptions
    _ | HcPkg.registerMultiInstance registerOptions
          -> die' verbosity "Registering multiple package instances is not yet supported for this compiler"
    LHC   -> LHC.registerPackage   verbosity      progdb packageDbs installedPkgInfo registerOptions
    UHC   -> UHC.registerPackage   verbosity comp progdb packageDbs installedPkgInfo
    JHC   -> notice verbosity "Registering for jhc (nothing to do)"
    HaskellSuite {} ->
      HaskellSuite.registerPackage verbosity      progdb packageDbs installedPkgInfo
    _    -> die' verbosity "Registering is not implemented for this compiler"

writeHcPkgRegisterScript :: Verbosity
                         -> [InstalledPackageInfo]
                         -> PackageDBStack
                         -> HcPkg.HcPkgInfo
                         -> IO ()
writeHcPkgRegisterScript verbosity ipis packageDbs hpi = do
  let genScript installedPkgInfo =
          let invocation  = HcPkg.registerInvocation hpi Verbosity.normal
                              packageDbs installedPkgInfo
                              HcPkg.defaultRegisterOptions
          in invocationAsSystemScript buildOS invocation
      scripts = map genScript ipis
      -- TODO: Do something more robust here
      regScript = unlines scripts

  info verbosity ("Creating package registration script: " ++ regScriptFileName)
  writeUTF8File regScriptFileName regScript
  setFileExecutable regScriptFileName

regScriptFileName :: FilePath
regScriptFileName = case buildOS of
                        Windows -> "register.bat"
                        _       -> "register.sh"


-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

-- | Construct 'InstalledPackageInfo' for a library in a package, given a set
-- of installation directories.
--
generalInstalledPackageInfo
  :: ([FilePath] -> [FilePath]) -- ^ Translate relative include dir paths to
                                -- absolute paths.
  -> PackageDescription
  -> AbiHash
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> InstallDirs FilePath
  -> InstalledPackageInfo
generalInstalledPackageInfo adjustRelIncDirs pkg abi_hash lib lbi clbi installDirs =
  IPI.InstalledPackageInfo {
    IPI.sourcePackageId    = packageId pkg,
    IPI.installedUnitId    = componentUnitId clbi,
    IPI.installedComponentId_ = componentComponentId clbi,
    IPI.instantiatedWith   = componentInstantiatedWith clbi,
    IPI.sourceLibName      = libName lib,
    IPI.compatPackageKey   = componentCompatPackageKey clbi,
    IPI.license            = license     pkg,
    IPI.copyright          = copyright   pkg,
    IPI.maintainer         = maintainer  pkg,
    IPI.author             = author      pkg,
    IPI.stability          = stability   pkg,
    IPI.homepage           = homepage    pkg,
    IPI.pkgUrl             = pkgUrl      pkg,
    IPI.synopsis           = synopsis    pkg,
    IPI.description        = description pkg,
    IPI.category           = category    pkg,
    IPI.abiHash            = abi_hash,
    IPI.indefinite         = componentIsIndefinite clbi,
    IPI.exposed            = libExposed  lib,
    IPI.exposedModules     = componentExposedModules clbi
                             -- add virtual modules into the list of exposed modules for the
                             -- package database as well.
                             ++ map (\name -> IPI.ExposedModule name Nothing) (virtualModules bi),
    IPI.hiddenModules      = otherModules bi,
    IPI.trusted            = IPI.trusted IPI.emptyInstalledPackageInfo,
    IPI.importDirs         = [ libdir installDirs | hasModules ],
    IPI.libraryDirs        = libdirs,
    IPI.libraryDynDirs     = dynlibdirs,
    IPI.dataDir            = datadir installDirs,
    IPI.hsLibraries        = extraBundledLibs bi
                             ++ if hasLibrary
                                then [getHSLibraryName (componentUnitId clbi)]
                                else [],
    IPI.extraLibraries     = extraLibs bi,
    IPI.extraGHCiLibraries = extraGHCiLibs bi,
    IPI.includeDirs        = absinc ++ adjustRelIncDirs relinc,
    IPI.includes           = includes bi,
    IPI.depends            = depends,
    IPI.abiDepends         = abi_depends,
    IPI.ccOptions          = [], -- Note. NOT ccOptions bi!
                                 -- We don't want cc-options to be propagated
                                 -- to C compilations in other packages.
    IPI.ldOptions          = ldOptions bi,
    IPI.frameworks         = frameworks bi,
    IPI.frameworkDirs      = extraFrameworkDirs bi,
    IPI.haddockInterfaces  = [haddockdir installDirs </> haddockName pkg],
    IPI.haddockHTMLs       = [htmldir installDirs],
    IPI.pkgRoot            = Nothing
  }
  where
    bi = libBuildInfo lib
    --TODO: unclear what the root cause of the
    -- duplication is, but we nub it here for now:
    depends = ordNub $ map fst (componentPackageDeps clbi)
    abi_depends = map add_abi depends
    add_abi uid = IPI.AbiDependency uid abi
      where
        abi = case Index.lookupUnitId (installedPkgs lbi) uid of
                Nothing -> error $
                  "generalInstalledPackageInfo: missing IPI for " ++ display uid
                Just ipi -> IPI.abiHash ipi
    (absinc, relinc) = partition isAbsolute (includeDirs bi)
    hasModules = not $ null (allLibModules lib clbi)
    comp = compiler lbi
    hasLibrary = (hasModules || not (null (cSources bi))
                             || not (null (asmSources bi))
                             || not (null (cmmSources bi))
                             || not (null (cxxSources bi))
                             || (not (null (jsSources bi)) &&
                                compilerFlavor comp == GHCJS))
               && not (componentIsIndefinite clbi)
    (libdirs, dynlibdirs)
      | not hasLibrary
      = (extraLibDirs bi, [])
      -- the dynamic-library-dirs defaults to the library-dirs if not specified,
      -- so this works whether the dynamic-library-dirs field is supported or not

      | libraryDynDirSupported comp
      = (libdir    installDirs : extraLibDirs bi,
         dynlibdir installDirs : extraLibDirs bi)

      | otherwise
      = (libdir installDirs : dynlibdir installDirs : extraLibDirs bi, [])
      -- the compiler doesn't understand the dynamic-library-dirs field so we
      -- add the dyn directory to the "normal" list in the library-dirs field

-- | Construct 'InstalledPackageInfo' for a library that is in place in the
-- build tree.
--
-- This function knows about the layout of in place packages.
--
inplaceInstalledPackageInfo :: FilePath -- ^ top of the build tree
                            -> FilePath -- ^ location of the dist tree
                            -> PackageDescription
                            -> AbiHash
                            -> Library
                            -> LocalBuildInfo
                            -> ComponentLocalBuildInfo
                            -> InstalledPackageInfo
inplaceInstalledPackageInfo inplaceDir distPref pkg abi_hash lib lbi clbi =
    generalInstalledPackageInfo adjustRelativeIncludeDirs
                                pkg abi_hash lib lbi clbi installDirs
  where
    adjustRelativeIncludeDirs = map (inplaceDir </>)
    libTargetDir = componentBuildDir lbi clbi
    installDirs =
      (absoluteComponentInstallDirs pkg lbi (componentUnitId clbi) NoCopyDest) {
        libdir     = inplaceDir </> libTargetDir,
        dynlibdir  = inplaceDir </> libTargetDir,
        datadir    = inplaceDir </> dataDir pkg,
        docdir     = inplaceDocdir,
        htmldir    = inplaceHtmldir,
        haddockdir = inplaceHtmldir
      }
    inplaceDocdir  = inplaceDir </> distPref </> "doc"
    inplaceHtmldir = inplaceDocdir </> "html" </> display (packageName pkg)


-- | Construct 'InstalledPackageInfo' for the final install location of a
-- library package.
--
-- This function knows about the layout of installed packages.
--
absoluteInstalledPackageInfo :: PackageDescription
                             -> AbiHash
                             -> Library
                             -> LocalBuildInfo
                             -> ComponentLocalBuildInfo
                             -> InstalledPackageInfo
absoluteInstalledPackageInfo pkg abi_hash lib lbi clbi =
    generalInstalledPackageInfo adjustReativeIncludeDirs
                                pkg abi_hash lib lbi clbi installDirs
  where
    -- For installed packages we install all include files into one dir,
    -- whereas in the build tree they may live in multiple local dirs.
    adjustReativeIncludeDirs _
      | null (installIncludes bi) = []
      | otherwise                 = [includedir installDirs]
    bi = libBuildInfo lib
    installDirs = absoluteComponentInstallDirs pkg lbi (componentUnitId clbi) NoCopyDest


relocatableInstalledPackageInfo :: PackageDescription
                                -> AbiHash
                                -> Library
                                -> LocalBuildInfo
                                -> ComponentLocalBuildInfo
                                -> FilePath
                                -> InstalledPackageInfo
relocatableInstalledPackageInfo pkg abi_hash lib lbi clbi pkgroot =
    generalInstalledPackageInfo adjustReativeIncludeDirs
                                pkg abi_hash lib lbi clbi installDirs
  where
    -- For installed packages we install all include files into one dir,
    -- whereas in the build tree they may live in multiple local dirs.
    adjustReativeIncludeDirs _
      | null (installIncludes bi) = []
      | otherwise                 = [includedir installDirs]
    bi = libBuildInfo lib

    installDirs = fmap (("${pkgroot}" </>) . shortRelativePath pkgroot)
                $ absoluteComponentInstallDirs pkg lbi (componentUnitId clbi) NoCopyDest

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg lbi regFlags = do
  let pkgid     = packageId pkg
      genScript = fromFlag (regGenScript regFlags)
      verbosity = fromFlag (regVerbosity regFlags)
      packageDb = fromFlagOrDefault (registrationPackageDB (withPackageDB lbi))
                                    (regPackageDB regFlags)
      unreg hpi =
        let invocation = HcPkg.unregisterInvocation
                           hpi Verbosity.normal packageDb pkgid
        in if genScript
             then writeFileAtomic unregScriptFileName
                    (BS.Char8.pack $ invocationAsSystemScript buildOS invocation)
             else runProgramInvocation verbosity invocation
  setupMessage verbosity "Unregistering" pkgid
  withHcPkg verbosity "unregistering is only implemented for GHC and GHCJS"
    (compiler lbi) (withPrograms lbi) unreg

unregScriptFileName :: FilePath
unregScriptFileName = case buildOS of
                          Windows -> "unregister.bat"
                          _       -> "unregister.sh"

internalPackageDBPath :: LocalBuildInfo -> FilePath -> FilePath
internalPackageDBPath lbi distPref =
      case compilerFlavor (compiler lbi) of
        UHC -> UHC.inplacePackageDbPath lbi
        _   -> distPref </> "package.conf.inplace"
