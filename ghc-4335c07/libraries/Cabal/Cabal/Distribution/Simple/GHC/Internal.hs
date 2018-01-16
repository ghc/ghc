{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.Internal
-- Copyright   :  Isaac Jones 2003-2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains functions shared by GHC (Distribution.Simple.GHC)
-- and GHC-derived compilers.

module Distribution.Simple.GHC.Internal (
        configureToolchain,
        getLanguages,
        getExtensions,
        targetPlatform,
        getGhcInfo,
        componentCcGhcOptions,
        componentCxxGhcOptions,
        componentGhcOptions,
        mkGHCiLibName,
        filterGhciFlags,
        ghcLookupProperty,
        getHaskellObjects,
        mkGhcOptPackages,
        substTopDir,
        checkPackageDbEnvVar,
        profDetailLevelFlag,
        -- * GHC platform and version strings
        ghcArchString,
        ghcOsString,
        ghcPlatformAndVersionString,
        -- * Constructing GHC environment files
        GhcEnvironmentFileEntry(..),
        writeGhcEnvironmentFile,
        simpleGhcEnvironmentFile,
        ghcEnvironmentFileName,
        renderGhcEnvironmentFile,
        renderGhcEnvironmentFileEntry,
 ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.GHC.ImplInfo
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Backpack
import Distribution.InstalledPackageInfo
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Compat.Exception
import Distribution.Lex
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.UnitId
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.System
import Distribution.Text ( display, simpleParse )
import Distribution.Utils.NubList ( toNubListR )
import Distribution.Verbosity
import Distribution.Compat.Stack
import Distribution.Version (Version)
import Language.Haskell.Extension

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory         ( getDirectoryContents, getTemporaryDirectory )
import System.Environment       ( getEnv )
import System.FilePath          ( (</>), (<.>), takeExtension
                                , takeDirectory, takeFileName)
import System.IO                ( hClose, hPutStrLn )

targetPlatform :: [(String, String)] -> Maybe Platform
targetPlatform ghcInfo = platformFromTriple =<< lookup "Target platform" ghcInfo

-- | Adjust the way we find and configure gcc and ld
--
configureToolchain :: GhcImplInfo
                   -> ConfiguredProgram
                   -> Map String String
                   -> ProgramDb
                   -> ProgramDb
configureToolchain _implInfo ghcProg ghcInfo =
    addKnownProgram gccProgram {
      programFindLocation = findProg gccProgramName extraGccPath,
      programPostConf     = configureGcc
    }
  . addKnownProgram ldProgram {
      programFindLocation = findProg ldProgramName extraLdPath,
      programPostConf     = configureLd
    }
  . addKnownProgram arProgram {
      programFindLocation = findProg arProgramName extraArPath
    }
  . addKnownProgram stripProgram {
      programFindLocation = findProg stripProgramName extraStripPath
    }
  where
    compilerDir = takeDirectory (programPath ghcProg)
    baseDir     = takeDirectory compilerDir
    mingwBinDir = baseDir </> "mingw" </> "bin"
    isWindows   = case buildOS of Windows -> True; _ -> False
    binPrefix   = ""

    maybeName :: Program -> Maybe FilePath -> String
    maybeName prog   = maybe (programName prog) (dropExeExtension . takeFileName)

    gccProgramName   = maybeName gccProgram   mbGccLocation
    ldProgramName    = maybeName ldProgram    mbLdLocation
    arProgramName    = maybeName arProgram    mbArLocation
    stripProgramName = maybeName stripProgram mbStripLocation

    mkExtraPath :: Maybe FilePath -> FilePath -> [FilePath]
    mkExtraPath mbPath mingwPath | isWindows = mbDir ++ [mingwPath]
                                 | otherwise = mbDir
      where
        mbDir = maybeToList . fmap takeDirectory $ mbPath

    extraGccPath   = mkExtraPath mbGccLocation   windowsExtraGccDir
    extraLdPath    = mkExtraPath mbLdLocation    windowsExtraLdDir
    extraArPath    = mkExtraPath mbArLocation    windowsExtraArDir
    extraStripPath = mkExtraPath mbStripLocation windowsExtraStripDir

    -- on Windows finding and configuring ghc's gcc & binutils is a bit special
    (windowsExtraGccDir, windowsExtraLdDir,
     windowsExtraArDir, windowsExtraStripDir) =
          let b = mingwBinDir </> binPrefix
          in  (b, b, b, b)

    findProg :: String -> [FilePath]
             -> Verbosity -> ProgramSearchPath
             -> IO (Maybe (FilePath, [FilePath]))
    findProg progName extraPath v searchpath =
        findProgramOnSearchPath v searchpath' progName
      where
        searchpath' = (map ProgramSearchPathDir extraPath) ++ searchpath

    -- Read tool locations from the 'ghc --info' output. Useful when
    -- cross-compiling.
    mbGccLocation   = Map.lookup "C compiler command" ghcInfo
    mbLdLocation    = Map.lookup "ld command" ghcInfo
    mbArLocation    = Map.lookup "ar command" ghcInfo
    mbStripLocation = Map.lookup "strip command" ghcInfo

    ccFlags        = getFlags "C compiler flags"
    -- GHC 7.8 renamed "Gcc Linker flags" to "C compiler link flags"
    -- and "Ld Linker flags" to "ld flags" (GHC #4862).
    gccLinkerFlags = getFlags "Gcc Linker flags" ++ getFlags "C compiler link flags"
    ldLinkerFlags  = getFlags "Ld Linker flags" ++ getFlags "ld flags"

    -- It appears that GHC 7.6 and earlier encode the tokenized flags as a
    -- [String] in these settings whereas later versions just encode the flags as
    -- String.
    --
    -- We first try to parse as a [String] and if this fails then tokenize the
    -- flags ourself.
    getFlags :: String -> [String]
    getFlags key =
        case Map.lookup key ghcInfo of
          Nothing -> []
          Just flags
            | (flags', ""):_ <- reads flags -> flags'
            | otherwise -> tokenizeQuotedWords flags

    configureGcc :: Verbosity -> ConfiguredProgram -> NoCallStackIO ConfiguredProgram
    configureGcc _v gccProg = do
      return gccProg {
        programDefaultArgs = programDefaultArgs gccProg
                             ++ ccFlags ++ gccLinkerFlags
      }

    configureLd :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureLd v ldProg = do
      ldProg' <- configureLd' v ldProg
      return ldProg' {
        programDefaultArgs = programDefaultArgs ldProg' ++ ldLinkerFlags
      }

    -- we need to find out if ld supports the -x flag
    configureLd' :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureLd' verbosity ldProg = do
      tempDir <- getTemporaryDirectory
      ldx <- withTempFile tempDir ".c" $ \testcfile testchnd ->
             withTempFile tempDir ".o" $ \testofile testohnd -> do
               hPutStrLn testchnd "int foo() { return 0; }"
               hClose testchnd; hClose testohnd
               runProgram verbosity ghcProg
                          [ "-hide-all-packages"
                          , "-c", testcfile
                          , "-o", testofile
                          ]
               withTempFile tempDir ".o" $ \testofile' testohnd' ->
                 do
                   hClose testohnd'
                   _ <- getProgramOutput verbosity ldProg
                     ["-x", "-r", testofile, "-o", testofile']
                   return True
                 `catchIO`   (\_ -> return False)
                 `catchExit` (\_ -> return False)
      if ldx
        then return ldProg { programDefaultArgs = ["-x"] }
        else return ldProg

getLanguages :: Verbosity -> GhcImplInfo -> ConfiguredProgram
             -> NoCallStackIO [(Language, String)]
getLanguages _ implInfo _
  -- TODO: should be using --supported-languages rather than hard coding
  | supportsHaskell2010 implInfo = return [(Haskell98,   "-XHaskell98")
                                          ,(Haskell2010, "-XHaskell2010")]
  | otherwise                    = return [(Haskell98,   "")]

getGhcInfo :: Verbosity -> GhcImplInfo -> ConfiguredProgram
           -> IO [(String, String)]
getGhcInfo verbosity _implInfo ghcProg = do
      xs <- getProgramOutput verbosity (suppressOverrideArgs ghcProg)
                 ["--info"]
      case reads xs of
        [(i, ss)]
          | all isSpace ss ->
              return i
        _ ->
          die' verbosity "Can't parse --info output of GHC"

getExtensions :: Verbosity -> GhcImplInfo -> ConfiguredProgram
              -> IO [(Extension, String)]
getExtensions verbosity implInfo ghcProg = do
    str <- getProgramOutput verbosity (suppressOverrideArgs ghcProg)
              ["--supported-languages"]
    let extStrs = if reportsNoExt implInfo
                  then lines str
                  else -- Older GHCs only gave us either Foo or NoFoo,
                       -- so we have to work out the other one ourselves
                       [ extStr''
                       | extStr <- lines str
                       , let extStr' = case extStr of
                                       'N' : 'o' : xs -> xs
                                       _              -> "No" ++ extStr
                       , extStr'' <- [extStr, extStr']
                       ]
    let extensions0 = [ (ext, "-X" ++ display ext)
                      | Just ext <- map simpleParse extStrs ]
        extensions1 = if alwaysNondecIndent implInfo
                      then -- ghc-7.2 split NondecreasingIndentation off
                           -- into a proper extension. Before that it
                           -- was always on.
                           (EnableExtension  NondecreasingIndentation, "") :
                           (DisableExtension NondecreasingIndentation, "") :
                           extensions0
                      else extensions0
    return extensions1

componentCcGhcOptions :: Verbosity -> GhcImplInfo -> LocalBuildInfo
                      -> BuildInfo -> ComponentLocalBuildInfo
                      -> FilePath -> FilePath
                      -> GhcOptions
componentCcGhcOptions verbosity _implInfo lbi bi clbi odir filename =
    mempty {
      -- Respect -v0, but don't crank up verbosity on GHC if
      -- Cabal verbosity is requested. For that, use --ghc-option=-v instead!
      ghcOptVerbosity      = toFlag (min verbosity normal),
      ghcOptMode           = toFlag GhcModeCompile,
      ghcOptInputFiles     = toNubListR [filename],

      ghcOptCppIncludePath = toNubListR $ [autogenComponentModulesDir lbi clbi
                                          ,autogenPackageModulesDir lbi
                                          ,odir]
                                          ++ PD.includeDirs bi,
      ghcOptHideAllPackages= toFlag True,
      ghcOptPackageDBs     = withPackageDB lbi,
      ghcOptPackages       = toNubListR $ mkGhcOptPackages clbi,
      ghcOptCcOptions      = toNubListR $
                             (case withOptimization lbi of
                                  NoOptimisation -> []
                                  _              -> ["-O2"]) ++
                             (case withDebugInfo lbi of
                                  NoDebugInfo   -> []
                                  MinimalDebugInfo -> ["-g1"]
                                  NormalDebugInfo  -> ["-g"]
                                  MaximalDebugInfo -> ["-g3"]) ++
                                  PD.ccOptions bi,
      ghcOptObjDir         = toFlag odir
    }


componentCxxGhcOptions :: Verbosity -> GhcImplInfo -> LocalBuildInfo
                      -> BuildInfo -> ComponentLocalBuildInfo
                      -> FilePath -> FilePath
                      -> GhcOptions
componentCxxGhcOptions verbosity _implInfo lbi bi cxxlbi odir filename =
    mempty {
      -- Respect -v0, but don't crank up verbosity on GHC if
      -- Cabal verbosity is requested. For that, use --ghc-option=-v instead!
      ghcOptVerbosity      = toFlag (min verbosity normal),
      ghcOptMode           = toFlag GhcModeCompile,
      ghcOptInputFiles     = toNubListR [filename],

      ghcOptCppIncludePath = toNubListR $ [autogenComponentModulesDir lbi cxxlbi
                                          ,autogenPackageModulesDir lbi
                                          ,odir]
                                          ++ PD.includeDirs bi,
      ghcOptHideAllPackages= toFlag True,
      ghcOptPackageDBs     = withPackageDB lbi,
      ghcOptPackages       = toNubListR $ mkGhcOptPackages cxxlbi,
      ghcOptCxxOptions     = toNubListR $
                             (case withOptimization lbi of
                                  NoOptimisation -> []
                                  _              -> ["-O2"]) ++
                             (case withDebugInfo lbi of
                                  NoDebugInfo   -> []
                                  MinimalDebugInfo -> ["-g1"]
                                  NormalDebugInfo  -> ["-g"]
                                  MaximalDebugInfo -> ["-g3"]) ++
                                  PD.cxxOptions bi,
      ghcOptObjDir         = toFlag odir
    }


componentGhcOptions :: Verbosity -> GhcImplInfo -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity implInfo lbi bi clbi odir =
    mempty {
      -- Respect -v0, but don't crank up verbosity on GHC if
      -- Cabal verbosity is requested. For that, use --ghc-option=-v instead!
      ghcOptVerbosity       = toFlag (min verbosity normal),
      ghcOptCabal           = toFlag True,
      ghcOptThisUnitId      = case clbi of
        LibComponentLocalBuildInfo { componentCompatPackageKey = pk }
          -> toFlag pk
        _ -> mempty,
      ghcOptThisComponentId = case clbi of
          LibComponentLocalBuildInfo { componentComponentId = cid
                                     , componentInstantiatedWith = insts } ->
              if null insts
                  then mempty
                  else toFlag cid
          _ -> mempty,
      ghcOptInstantiatedWith = case clbi of
        LibComponentLocalBuildInfo { componentInstantiatedWith = insts }
          -> insts
        _ -> [],
      ghcOptNoCode          = toFlag $ componentIsIndefinite clbi,
      ghcOptHideAllPackages = toFlag True,
      ghcOptWarnMissingHomeModules = toFlag $ flagWarnMissingHomeModules implInfo,
      ghcOptPackageDBs      = withPackageDB lbi,
      ghcOptPackages        = toNubListR $ mkGhcOptPackages clbi,
      ghcOptSplitObjs       = toFlag (splitObjs lbi),
      ghcOptSourcePathClear = toFlag True,
      ghcOptSourcePath      = toNubListR $ [odir] ++ (hsSourceDirs bi)
                                           ++ [autogenComponentModulesDir lbi clbi]
                                           ++ [autogenPackageModulesDir lbi],
      ghcOptCppIncludePath  = toNubListR $ [autogenComponentModulesDir lbi clbi
                                           ,autogenPackageModulesDir lbi
                                           ,odir]
                                           ++ PD.includeDirs bi,
      ghcOptCppOptions      = toNubListR $ cppOptions bi,
      ghcOptCppIncludes     = toNubListR $
                              [autogenComponentModulesDir lbi clbi </> cppHeaderName],
      ghcOptFfiIncludes     = toNubListR $ PD.includes bi,
      ghcOptObjDir          = toFlag odir,
      ghcOptHiDir           = toFlag odir,
      ghcOptStubDir         = toFlag odir,
      ghcOptOutputDir       = toFlag odir,
      ghcOptOptimisation    = toGhcOptimisation (withOptimization lbi),
      ghcOptDebugInfo       = toFlag (withDebugInfo lbi),
      ghcOptExtra           = toNubListR $ hcOptions GHC bi,
      ghcOptExtraPath       = toNubListR $ exe_paths,
      ghcOptLanguage        = toFlag (fromMaybe Haskell98 (defaultLanguage bi)),
      -- Unsupported extensions have already been checked by configure
      ghcOptExtensions      = toNubListR $ usedExtensions bi,
      ghcOptExtensionMap    = Map.fromList . compilerExtensions $ (compiler lbi)
    }
  where
    toGhcOptimisation NoOptimisation      = mempty --TODO perhaps override?
    toGhcOptimisation NormalOptimisation  = toFlag GhcNormalOptimisation
    toGhcOptimisation MaximumOptimisation = toFlag GhcMaximumOptimisation

    exe_paths = [ componentBuildDir lbi (targetCLBI exe_tgt)
                | uid <- componentExeDeps clbi
                -- TODO: Ugh, localPkgDescr
                , Just exe_tgt <- [unitIdTarget' (localPkgDescr lbi) lbi uid] ]

-- | Strip out flags that are not supported in ghci
filterGhciFlags :: [String] -> [String]
filterGhciFlags = filter supported
  where
    supported ('-':'O':_) = False
    supported "-debug"    = False
    supported "-threaded" = False
    supported "-ticky"    = False
    supported "-eventlog" = False
    supported "-prof"     = False
    supported "-unreg"    = False
    supported _           = True

mkGHCiLibName :: UnitId -> String
mkGHCiLibName lib = getHSLibraryName lib <.> "o"

ghcLookupProperty :: String -> Compiler -> Bool
ghcLookupProperty prop comp =
  case Map.lookup prop (compilerProperties comp) of
    Just "YES" -> True
    _          -> False

-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: GhcImplInfo -> Library -> LocalBuildInfo
                  -> ComponentLocalBuildInfo
                  -> FilePath -> String -> Bool -> NoCallStackIO [FilePath]
getHaskellObjects _implInfo lib lbi clbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let splitSuffix = "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- allLibModules lib clbi ]
        objss <- traverse getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- allLibModules lib clbi ]

mkGhcOptPackages :: ComponentLocalBuildInfo
                 -> [(OpenUnitId, ModuleRenaming)]
mkGhcOptPackages = componentIncludes

substTopDir :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
substTopDir topDir ipo
 = ipo {
       InstalledPackageInfo.importDirs
           = map f (InstalledPackageInfo.importDirs ipo),
       InstalledPackageInfo.libraryDirs
           = map f (InstalledPackageInfo.libraryDirs ipo),
       InstalledPackageInfo.includeDirs
           = map f (InstalledPackageInfo.includeDirs ipo),
       InstalledPackageInfo.frameworkDirs
           = map f (InstalledPackageInfo.frameworkDirs ipo),
       InstalledPackageInfo.haddockInterfaces
           = map f (InstalledPackageInfo.haddockInterfaces ipo),
       InstalledPackageInfo.haddockHTMLs
           = map f (InstalledPackageInfo.haddockHTMLs ipo)
   }
    where f ('$':'t':'o':'p':'d':'i':'r':rest) = topDir ++ rest
          f x = x

-- Cabal does not use the environment variable GHC{,JS}_PACKAGE_PATH; let
-- users know that this is the case. See ticket #335. Simply ignoring it is
-- not a good idea, since then ghc and cabal are looking at different sets
-- of package DBs and chaos is likely to ensue.
--
-- An exception to this is when running cabal from within a `cabal exec`
-- environment. In this case, `cabal exec` will set the
-- CABAL_SANDBOX_PACKAGE_PATH to the same value that it set
-- GHC{,JS}_PACKAGE_PATH to. If that is the case it is OK to allow
-- GHC{,JS}_PACKAGE_PATH.
checkPackageDbEnvVar :: Verbosity -> String -> String -> IO ()
checkPackageDbEnvVar verbosity compilerName packagePathEnvVar = do
    mPP <- lookupEnv packagePathEnvVar
    when (isJust mPP) $ do
        mcsPP <- lookupEnv "CABAL_SANDBOX_PACKAGE_PATH"
        unless (mPP == mcsPP) abort
    where
        lookupEnv :: String -> NoCallStackIO (Maybe String)
        lookupEnv name = (Just `fmap` getEnv name)
                         `catchIO` const (return Nothing)
        abort =
            die' verbosity $ "Use of " ++ compilerName ++ "'s environment variable "
               ++ packagePathEnvVar ++ " is incompatible with Cabal. Use the "
               ++ "flag --package-db to specify a package database (it can be "
               ++ "used multiple times)."

        _ = callStack -- TODO: output stack when erroring

profDetailLevelFlag :: Bool -> ProfDetailLevel -> Flag GhcProfAuto
profDetailLevelFlag forLib mpl =
    case mpl of
      ProfDetailNone                -> mempty
      ProfDetailDefault | forLib    -> toFlag GhcProfAutoExported
                        | otherwise -> toFlag GhcProfAutoToplevel
      ProfDetailExportedFunctions   -> toFlag GhcProfAutoExported
      ProfDetailToplevelFunctions   -> toFlag GhcProfAutoToplevel
      ProfDetailAllFunctions        -> toFlag GhcProfAutoAll
      ProfDetailOther _             -> mempty


-- -----------------------------------------------------------------------------
-- GHC platform and version strings

-- | GHC's rendering of it's host or target 'Arch' as used in its platform
-- strings and certain file locations (such as user package db location).
--
ghcArchString :: Arch -> String
ghcArchString PPC   = "powerpc"
ghcArchString PPC64 = "powerpc64"
ghcArchString other = display other

-- | GHC's rendering of it's host or target 'OS' as used in its platform
-- strings and certain file locations (such as user package db location).
--
ghcOsString :: OS -> String
ghcOsString Windows = "mingw32"
ghcOsString OSX     = "darwin"
ghcOsString Solaris = "solaris2"
ghcOsString other   = display other

-- | GHC's rendering of it's platform and compiler version string as used in
-- certain file locations (such as user package db location).
-- For example @x86_64-linux-7.10.4@
--
ghcPlatformAndVersionString :: Platform -> Version -> String
ghcPlatformAndVersionString (Platform arch os) version =
    intercalate "-" [ ghcArchString arch, ghcOsString os, display version ]


-- -----------------------------------------------------------------------------
-- Constructing GHC environment files

-- | The kinds of entries we can stick in a @.ghc.environment@ file.
--
data GhcEnvironmentFileEntry =
       GhcEnvFileComment   String     -- ^ @-- a comment@
     | GhcEnvFilePackageId UnitId     -- ^ @package-id foo-1.0-4fe301a...@
     | GhcEnvFilePackageDb PackageDB  -- ^ @global-package-db@,
                                      --   @user-package-db@ or
                                      --   @package-db blah/package.conf.d/@
     | GhcEnvFileClearPackageDbStack  -- ^ @clear-package-db@

-- | Make entries for a GHC environment file based on a 'PackageDBStack' and
-- a bunch of package (unit) ids.
--
-- If you need to do anything more complicated then either use this as a basis
-- and add more entries, or just make all the entries directly.
--
simpleGhcEnvironmentFile :: PackageDBStack
                         -> [UnitId]
                         -> [GhcEnvironmentFileEntry]
simpleGhcEnvironmentFile packageDBs pkgids =
    GhcEnvFileClearPackageDbStack
  : map GhcEnvFilePackageDb packageDBs
 ++ map GhcEnvFilePackageId pkgids

-- | Write a @.ghc.environment-$arch-$os-$ver@ file in the given directory.
--
-- The 'Platform' and GHC 'Version' are needed as part of the file name.
--
-- Returns the name of the file written.
writeGhcEnvironmentFile :: FilePath  -- ^ directory in which to put it
                        -> Platform  -- ^ the GHC target platform
                        -> Version   -- ^ the GHC version
                        -> [GhcEnvironmentFileEntry] -- ^ the content
                        -> NoCallStackIO FilePath
writeGhcEnvironmentFile directory platform ghcversion entries = do
    writeFileAtomic envfile . BS.pack . renderGhcEnvironmentFile $ entries
    return envfile
  where
    envfile = directory </> ghcEnvironmentFileName platform ghcversion

-- | The @.ghc.environment-$arch-$os-$ver@ file name
--
ghcEnvironmentFileName :: Platform -> Version -> FilePath
ghcEnvironmentFileName platform ghcversion =
    ".ghc.environment." ++ ghcPlatformAndVersionString platform ghcversion

-- | Render a bunch of GHC environment file entries
--
renderGhcEnvironmentFile :: [GhcEnvironmentFileEntry] -> String
renderGhcEnvironmentFile =
    unlines . map renderGhcEnvironmentFileEntry

-- | Render an individual GHC environment file entry
--
renderGhcEnvironmentFileEntry :: GhcEnvironmentFileEntry -> String
renderGhcEnvironmentFileEntry entry = case entry of
    GhcEnvFileComment   comment   -> format comment
      where format = intercalate "\n" . map ("-- " ++) . lines
    GhcEnvFilePackageId pkgid     -> "package-id " ++ display pkgid
    GhcEnvFilePackageDb pkgdb     ->
      case pkgdb of
        GlobalPackageDB           -> "global-package-db"
        UserPackageDB             -> "user-package-db"
        SpecificPackageDB dbfile  -> "package-db " ++ dbfile
    GhcEnvFileClearPackageDbStack -> "clear-package-db"

