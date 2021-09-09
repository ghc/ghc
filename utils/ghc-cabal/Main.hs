{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns    #-}

module Main (main) where

import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Check hiding (doesFileExist)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Package
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.GHC
import Distribution.Simple.Program
import Distribution.Simple.Program.HcPkg
import Distribution.Simple.Setup (ConfigFlags(configStripLibs), fromFlagOrDefault, toFlag)
import Distribution.Simple.Utils (defaultPackageDesc, findHookedPackageDesc, writeFileAtomic,
                                  toUTF8LBS)
import Distribution.Simple.Build (writeAutogenFiles)
import Distribution.Simple.Register
import qualified Distribution.Compat.Graph as Graph
import Distribution.Text
import Distribution.Types.MungedPackageId
import Distribution.Types.LocalBuildInfo
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Utils.Path (getSymbolicPath)

import Control.Exception (bracket)
import Control.Monad
import Control.Applicative ((<|>))
import Data.List (nub, intercalate, isPrefixOf, isSuffixOf)
import Data.Maybe
import Data.Char (isSpace)
import System.IO
import System.Directory (setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.Environment
import System.Exit      (exitWith, ExitCode(..))
import System.FilePath

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          args <- getArgs
          case args of
              "hscolour" : dir : distDir : args' ->
                  runHsColour dir distDir args'
              "check" : dir : [] ->
                  doCheck dir
              "copy" : dir : distDir
                     : strip : myDestDir : myPrefix : myLibdir : myDocdir
                     : ghcLibWays : args' ->
                  doCopy dir distDir
                         strip myDestDir myPrefix myLibdir myDocdir
                         ("dyn" `elem` words ghcLibWays)
                         args'
              "register" : dir : distDir : ghc : ghcpkg : topdir
                         : myDestDir : myPrefix : myLibdir : myDocdir
                         : relocatableBuild : args' ->
                  doRegister dir distDir ghc ghcpkg topdir
                             myDestDir myPrefix myLibdir myDocdir
                             relocatableBuild args'
              "configure" : dir : distDir : config_args ->
                  generate dir distDir config_args
              "sdist" : dir : distDir : [] ->
                  doSdist dir distDir
              ["--version"] ->
                  defaultMainArgs ["--version"]
              _ -> die syntax_error

syntax_error :: [String]
syntax_error =
    ["syntax: ghc-cabal configure <directory> <distdir> <args>...",
     "        ghc-cabal copy <directory> <distdir> <strip> <destdir> <prefix> <libdir> <docdir> <libways> <args>...",
     "        ghc-cabal register <directory> <distdir> <ghc> <ghcpkg> <topdir> <destdir> <prefix> <libdir> <docdir> <relocatable> <args>...",
     "        ghc-cabal hscolour <directory> <distdir> <args>...",
     "        ghc-cabal check <directory>",
     "        ghc-cabal sdist <directory> <distdir>",
     "        ghc-cabal --version"]

die :: [String] -> IO a
die errs = do mapM_ (hPutStrLn stderr) errs
              exitWith (ExitFailure 1)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory io
 = bracket (getCurrentDirectory) (setCurrentDirectory)
           (const (setCurrentDirectory directory >> io))

-- We need to use the autoconfUserHooks, as the packages that use
-- configure can create a .buildinfo file, and we need any info that
-- ends up in it.
userHooks :: UserHooks
userHooks = autoconfUserHooks

runDefaultMain :: IO ()
runDefaultMain
 = do let verbosity = normal
      gpdFile <- defaultPackageDesc verbosity
      gpd <- readGenericPackageDescription verbosity gpdFile
      case buildType (flattenPackageDescription gpd) of
          Configure -> defaultMainWithHooks autoconfUserHooks
          -- time has a "Custom" Setup.hs, but it's actually Configure
          -- plus a "./Setup test" hook. However, Cabal is also
          -- "Custom", but doesn't have a configure script.
          Custom ->
              do configureExists <- doesFileExist "configure"
                 if configureExists
                     then defaultMainWithHooks autoconfUserHooks
                     else defaultMain
          -- not quite right, but good enough for us:
          _ -> defaultMain

doSdist :: FilePath -> FilePath -> IO ()
doSdist directory distDir
 = withCurrentDirectory directory
 $ withArgs (["sdist", "--builddir", distDir])
            runDefaultMain

doCheck :: FilePath -> IO ()
doCheck directory
 = withCurrentDirectory directory
 $ do let verbosity = normal
      gpdFile <- defaultPackageDesc verbosity
      gpd <- readGenericPackageDescription verbosity gpdFile
      case filter isFailure $ checkPackage gpd Nothing of
          []   -> return ()
          errs -> mapM_ print errs >> exitWith (ExitFailure 1)
    where isFailure (PackageDistSuspicious {}) = False
          isFailure (PackageDistSuspiciousWarn {}) = False
          isFailure _ = True

runHsColour :: FilePath -> FilePath -> [String] -> IO ()
runHsColour directory distdir args
 = withCurrentDirectory directory
 $ defaultMainArgs ("hscolour" : "--builddir" : distdir : args)

doCopy :: FilePath -> FilePath
       -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Bool
       -> [String]
       -> IO ()
doCopy directory distDir
       strip myDestDir myPrefix myLibdir myDocdir withSharedLibs
       args
 = withCurrentDirectory directory $ do
     let copyArgs = ["copy", "--builddir", distDir]
                 ++ (if null myDestDir
                     then []
                     else ["--destdir", myDestDir])
                 ++ args
         copyHooks = userHooks {
                         copyHook = modHook False
                                  $ copyHook userHooks
                     }

     defaultMainWithHooksArgs copyHooks copyArgs
    where
      modHook relocatableBuild f pd lbi us flags
       = do let verbosity = normal
                idts = updateInstallDirTemplates relocatableBuild
                                                 myPrefix myLibdir myDocdir
                                                 (installDirTemplates lbi)
                progs = withPrograms lbi
                stripProgram' = stripProgram {
                    programFindLocation = \_ _ -> return (Just (strip,[])) }

            progs' <- configureProgram verbosity stripProgram' progs
            let lbi' = lbi {
                               withPrograms = progs',
                               installDirTemplates = idts,
                               configFlags = cfg,
                               stripLibs = fromFlagOrDefault False (configStripLibs cfg),
                               withSharedLib = withSharedLibs
                           }

                -- This hack allows to interpret the "strip"
                -- command-line argument being set to ':' to signify
                -- disabled library stripping
                cfg | strip == ":" = (configFlags lbi) { configStripLibs = toFlag False }
                    | otherwise    = configFlags lbi

            f pd lbi' us flags

doRegister :: FilePath -> FilePath -> FilePath -> FilePath
           -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath
           -> String -> [String]
           -> IO ()
doRegister directory distDir ghc ghcpkg topdir
           myDestDir myPrefix myLibdir myDocdir
           relocatableBuildStr args
 = withCurrentDirectory directory $ do
     relocatableBuild <- case relocatableBuildStr of
                         "YES" -> return True
                         "NO"  -> return False
                         _ -> die ["Bad relocatableBuildStr: " ++
                                   show relocatableBuildStr]
     let regArgs = "register" : "--builddir" : distDir : args
         regHooks = userHooks {
                        regHook = modHook relocatableBuild
                                $ regHook userHooks
                    }

     defaultMainWithHooksArgs regHooks  regArgs
    where
      modHook relocatableBuild f pd lbi us flags
       = do let verbosity = normal
                idts = updateInstallDirTemplates relocatableBuild
                                                 myPrefix myLibdir myDocdir
                                                 (installDirTemplates lbi)
                progs = withPrograms lbi
                ghcpkgconf = topdir </> "package.conf.d"
                ghcProgram' = ghcProgram {
                    programPostConf = \_ cp -> return cp { programDefaultArgs = ["-B" ++ topdir] },
                    programFindLocation = \_ _ -> return (Just (ghc,[])) }
                ghcPkgProgram' = ghcPkgProgram {
                    programPostConf = \_ cp -> return cp { programDefaultArgs =
                                                                ["--global-package-db", ghcpkgconf]
                                                                ++ ["--force" | not (null myDestDir) ] },
                    programFindLocation = \_ _ -> return (Just (ghcpkg,[])) }
                configurePrograms ps conf = foldM (flip (configureProgram verbosity)) conf ps

            progs' <- configurePrograms [ghcProgram', ghcPkgProgram'] progs
            instInfos <- dump (hcPkgInfo progs') verbosity GlobalPackageDB
            let installedPkgs' = PackageIndex.fromList instInfos
            let lbi' = lbi {
                               installedPkgs = installedPkgs',
                               installDirTemplates = idts,
                               withPrograms = progs'
                           }
            f pd lbi' us flags

updateInstallDirTemplates :: Bool -> FilePath -> FilePath -> FilePath
                          -> InstallDirTemplates
                          -> InstallDirTemplates
updateInstallDirTemplates relocatableBuild myPrefix myLibdir myDocdir idts
    = idts {
          prefix    = toPathTemplate $
                          if relocatableBuild
                          then "$topdir"
                          else myPrefix,
          libdir    = toPathTemplate $
                          if relocatableBuild
                          then "$topdir"
                          else myLibdir,
          dynlibdir = toPathTemplate $
                          (if relocatableBuild
                          then "$topdir"
                          else myLibdir) </> "$libname",
          libsubdir = toPathTemplate "$libname",
          docdir    = toPathTemplate $
                          if relocatableBuild
                          then "$topdir/../doc/html/libraries/$pkgid"
                          else (myDocdir </> "$pkgid"),
          htmldir   = toPathTemplate "$docdir"
      }

externalPackageDeps :: LocalBuildInfo -> [(UnitId, MungedPackageId)]
externalPackageDeps lbi =
    -- TODO:  what about non-buildable components?
    nub [ (ipkgid, pkgid)
        | clbi            <- Graph.toList (componentGraph lbi)
        , (ipkgid, pkgid) <- componentPackageDeps clbi
        , not (internal ipkgid) ]
  where
    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal ipkgid = any ((==ipkgid) . componentUnitId) (Graph.toList (componentGraph lbi))

generate :: FilePath -> FilePath -> [String] -> IO ()
generate directory distdir config_args
 = withCurrentDirectory directory
 $ do let verbosity = normal
      -- XXX We shouldn't just configure with the default flags
      -- XXX And this, and thus the "getPersistBuildConfig distdir" below,
      -- aren't going to work when the deps aren't built yet
      withArgs (["configure", "--distdir", distdir, "--ipid", "$pkg-$version"] ++ config_args)
               runDefaultMain

      lbi <- getPersistBuildConfig distdir
      let pd0 = localPkgDescr lbi

      writePersistBuildConfig distdir lbi

      hooked_bi <-
           if (buildType pd0 == Configure) || (buildType pd0 == Custom)
           then do
              cwd <- getCurrentDirectory
              -- Try to find the .buildinfo in the $dist/build folder where
              -- cabal 2.2+ will expect it, but fallback to the old default
              -- location if we don't find any.  This is the case of the
              -- bindist, which doesn't ship the $dist/build folder.
              maybe_infoFile <- findHookedPackageDesc verbosity (cwd </> distdir </> "build")
                                <|> fmap Just (defaultPackageDesc verbosity)
              case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> readHookedBuildInfo verbosity infoFile
           else
              return emptyHookedBuildInfo

      let pd = updatePackageDescription hooked_bi pd0

      -- generate Paths_<pkg>.hs and cabal-macros.h
      withAllComponentsInBuildOrder pd lbi $ \_ clbi ->
        writeAutogenFiles verbosity pd lbi clbi

      -- generate inplace-pkg-config
      withLibLBI pd lbi $ \lib clbi ->
          do cwd <- getCurrentDirectory
             let fixupIncludeDir dir | cwd `isPrefixOf` dir = [dir, cwd </> distdir </> "build" ++ drop (length cwd) dir]
                                     | otherwise            = [dir]
             let ipid = mkUnitId (display (packageId pd))
             let installedPkgInfo = inplaceInstalledPackageInfo cwd distdir
                                        pd (mkAbiHash "inplace") lib lbi clbi
                 final_ipi = installedPkgInfo {
                                 Installed.installedUnitId = ipid,
                                 Installed.compatPackageKey = display (packageId pd),
                                 Installed.includeDirs = concatMap fixupIncludeDir (Installed.includeDirs installedPkgInfo)
                             }
                 content = Installed.showInstalledPackageInfo final_ipi ++ "\n"
             writeFileAtomic (distdir </> "inplace-pkg-config")
                             (toUTF8LBS content)

      let
          comp = compiler lbi
          libBiModules lib = (libBuildInfo lib, foldMap (allLibModules lib) (componentNameCLBIs lbi $ CLibName defaultLibName))
          exeBiModules exe = (buildInfo exe, ModuleName.main : exeModules exe)
          biModuless :: [(BuildInfo, [ModuleName.ModuleName])]
          biModuless = (map libBiModules . maybeToList $ library pd)
                    ++ (map exeBiModules $ executables pd)
          buildableBiModuless = filter isBuildable biModuless
              where isBuildable (bi', _) = buildable bi'
          (bi, modules) = case buildableBiModuless of
                          [] -> error "No buildable component found"
                          [biModules] -> biModules
                          _ -> error ("XXX ghc-cabal can't handle " ++
                                      "more than one buildinfo yet")
          -- XXX Another Just...
          Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)

          dep_pkgs = PackageIndex.topologicalOrder (packageHacks (installedPkgs lbi))
          forDeps f = concatMap f dep_pkgs

          -- copied from Distribution.Simple.PreProcess.ppHsc2Hs
          packageHacks = case compilerFlavor (compiler lbi) of
            GHC -> hackRtsPackage
            _   -> id
          -- We don't link in the actual Haskell libraries of our
          -- dependencies, so the -u flags in the ldOptions of the rts
          -- package mean linking fails on OS X (it's ld is a tad
          -- stricter than gnu ld). Thus we remove the ldOptions for
          -- GHC's rts package:
          hackRtsPackage index =
            case PackageIndex.lookupPackageName index (mkPackageName "rts") of
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

          dep_ids  = map snd (externalPackageDeps lbi)
          deps     = map display dep_ids
          dep_direct = map (fromMaybe (error "ghc-cabal: dep_keys failed")
                           . PackageIndex.lookupUnitId
                                            (installedPkgs lbi)
                           . fst)
                       . externalPackageDeps
                       $ lbi
          dep_ipids = map (display . Installed.installedUnitId) dep_direct
          depLibNames
            | packageKeySupported comp = dep_ipids
            | otherwise = deps
          depNames = map (display . mungedName) dep_ids

          transitive_dep_ids = map Installed.sourcePackageId dep_pkgs
          transitiveDeps = map display transitive_dep_ids
          transitiveDepLibNames
            | packageKeySupported comp = map fixupRtsLibName transitiveDeps
            | otherwise = transitiveDeps
          fixupRtsLibName x | "rts-" `isPrefixOf` x = "rts"
          fixupRtsLibName x = x
          transitiveDepNames = map (display . packageName) transitive_dep_ids

          -- Note [Msys2 path translation bug].
          -- Msys2 has an annoying bug in their path conversion code.
          -- Officially anything starting with a drive letter should not be
          -- subjected to path translations, however it seems to only consider
          -- E:\\ and E:// to be Windows paths.  Mixed mode paths such as E:/
          -- that are produced here get corrupted.
          --
          -- Tamar@Rage /t/translate> ./a.exe -optc-I"E://ghc-dev/msys64/"
          -- path: -optc-IE://ghc-dev/msys64/
          -- Tamar@Rage /t/translate> ./a.exe -optc-I"E:ghc-dev/msys64/"
          -- path: -optc-IE:ghc-dev/msys64/
          -- Tamar@Rage /t/translate> ./a.exe -optc-I"E:\ghc-dev/msys64/"
          -- path: -optc-IE:\ghc-dev/msys64/
          --
          -- As such, let's just normalize the filepaths which is a good thing
          -- to do anyway.
          libraryDirs = map normalise $ forDeps Installed.libraryDirs
          -- The mkLibraryRelDir function is a bit of a hack.
          -- Ideally it should be handled in the makefiles instead.
          mkLibraryRelDir "rts"        = "rts/dist-install/build"
          mkLibraryRelDir "ghc"        = "compiler/stage2/build"
          mkLibraryRelDir "Cabal"      = "libraries/Cabal/Cabal/dist-install/build"
          mkLibraryRelDir "containers" = "libraries/containers/containers/dist-install/build"
          mkLibraryRelDir l            = "libraries/" ++ l ++ "/dist-install/build"
          libraryRelDirs = map mkLibraryRelDir transitiveDepNames

          -- this is a hack to accommodate Cabal 2.2+ more hygenic
          -- generated data.   We'll inject `dist-install/build` after
          -- before the `include` directory, if any.
          injectDistInstall :: FilePath -> [FilePath]
          injectDistInstall x | takeBaseName x == "include" = [x, takeDirectory x ++ "/dist-install/build/" ++ takeBaseName x]
          injectDistInstall x = [x]

      -- See Note [Msys2 path translation bug].
      wrappedIncludeDirs <- wrap $ map normalise $ concatMap injectDistInstall $ forDeps Installed.includeDirs

      let variablePrefix = directory ++ '_':distdir
          mods      = map display modules
          otherMods = map display (otherModules bi)
          buildDir' = map (\c -> if c=='\\' then '/' else c) $ buildDir lbi
      let xs = [variablePrefix ++ "_VERSION = " ++ display (pkgVersion (package pd)),
                -- TODO: move inside withLibLBI
                variablePrefix ++ "_COMPONENT_ID = " ++ localCompatPackageKey lbi,
                variablePrefix ++ "_MODULES = " ++ unwords mods,
                variablePrefix ++ "_HIDDEN_MODULES = " ++ unwords otherMods,
                variablePrefix ++ "_SYNOPSIS =" ++ (unwords $ lines $ fromShortText $ synopsis pd),
                variablePrefix ++ "_HS_SRC_DIRS = " ++ unwords (map getSymbolicPath $ hsSourceDirs bi),
                variablePrefix ++ "_DEPS = " ++ unwords deps,
                variablePrefix ++ "_DEP_IPIDS = " ++ unwords dep_ipids,
                variablePrefix ++ "_DEP_NAMES = " ++ unwords depNames,
                variablePrefix ++ "_DEP_COMPONENT_IDS = " ++ unwords depLibNames,
                variablePrefix ++ "_TRANSITIVE_DEP_NAMES = " ++ unwords transitiveDepNames,
                variablePrefix ++ "_TRANSITIVE_DEP_COMPONENT_IDS = " ++ unwords transitiveDepLibNames,
                variablePrefix ++ "_INCLUDE_DIRS = " ++ unwords (  [ dir | dir <- includeDirs bi ]
                                                                ++ [ buildDir' ++ "/" ++ dir | dir <- includeDirs bi
                                                                                             , not (isAbsolute dir)]),
                variablePrefix ++ "_INCLUDES = " ++ unwords (includes bi),
                variablePrefix ++ "_INSTALL_INCLUDES = " ++ unwords (installIncludes bi),
                variablePrefix ++ "_EXTRA_LIBRARIES = " ++ unwords (extraLibs bi),
                variablePrefix ++ "_EXTRA_LIBDIRS = " ++ unwords (extraLibDirs bi),
                variablePrefix ++ "_S_SRCS = " ++ unwords (asmSources bi),
                variablePrefix ++ "_C_SRCS  = " ++ unwords (cSources bi),
                variablePrefix ++ "_CXX_SRCS  = " ++ unwords (cxxSources bi),
                variablePrefix ++ "_CMM_SRCS = " ++ unwords (cmmSources bi),
                variablePrefix ++ "_DATA_FILES = "    ++ unwords (dataFiles pd),
                -- XXX This includes things it shouldn't, like:
                -- -odir dist-bootstrapping/build
                variablePrefix ++ "_HC_OPTS = " ++ escapeArgs
                       (   programDefaultArgs ghcProg
                        ++ hcOptions GHC bi
                        ++ languageToFlags (compiler lbi) (defaultLanguage bi)
                        ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
                        ++ programOverrideArgs ghcProg),
                variablePrefix ++ "_CC_OPTS = "                        ++ unwords (ccOptions bi),
                variablePrefix ++ "_CPP_OPTS = "                       ++ unwords (cppOptions bi),
                variablePrefix ++ "_LD_OPTS = "                        ++ unwords (ldOptions bi),
                variablePrefix ++ "_DEP_INCLUDE_DIRS_SINGLE_QUOTED = " ++ unwords wrappedIncludeDirs,
                variablePrefix ++ "_DEP_CC_OPTS = "                    ++ unwords (forDeps Installed.ccOptions),
                variablePrefix ++ "_DEP_LIB_DIRS_SEARCHPATH = "        ++ mkSearchPath libraryDirs,
                variablePrefix ++ "_DEP_LIB_REL_DIRS = "               ++ unwords libraryRelDirs,
                variablePrefix ++ "_DEP_LIB_REL_DIRS_SEARCHPATH = "    ++ mkSearchPath libraryRelDirs,
                variablePrefix ++ "_DEP_LD_OPTS = "                    ++ unwords (forDeps Installed.ldOptions),
                variablePrefix ++ "_BUILD_GHCI_LIB = "                 ++ boolToYesNo (withGHCiLib lbi),
                "",
                -- Sometimes we need to modify the automatically-generated package-data.mk
                -- bindings in a special way for the GHC build system, so allow that here:
                "$(eval $(" ++ directory ++ "_PACKAGE_MAGIC))"
                ]
      writeFile (distdir ++ "/package-data.mk") $ unlines xs

      writeFileUtf8 (distdir ++ "/haddock-prologue.txt") $ fromShortText $
          if null (fromShortText $ description pd) then synopsis pd
                                                   else description pd
  where
     wrap = mapM wrap1
     wrap1 s
      | null s        = die ["Wrapping empty value"]
      | '\'' `elem` s = die ["Single quote in value to be wrapped:", s]
      -- We want to be able to assume things like <space><quote> is the
      -- start of a value, so check there are no spaces in confusing
      -- positions
      | head s == ' ' = die ["Leading space in value to be wrapped:", s]
      | last s == ' ' = die ["Trailing space in value to be wrapped:", s]
      | otherwise     = return ("\'" ++ s ++ "\'")
     mkSearchPath = intercalate [searchPathSeparator]
     boolToYesNo True = "YES"
     boolToYesNo False = "NO"

     -- | Version of 'writeFile' that always uses UTF8 encoding
     writeFileUtf8 f txt = withFile f WriteMode $ \hdl -> do
         hSetEncoding hdl utf8
         hPutStr hdl txt

-- | Like GHC.ResponseFile.escapeArgs but uses spaces instead of newlines to seperate arguments
escapeArgs :: [String] -> String
escapeArgs = unwords . map escapeArg

escapeArg :: String -> String
escapeArg = foldr escape ""

escape :: Char -> String -> String
escape c cs
  | isSpace c || c `elem` ['\\','\'','#','"']
    = '\\':c:cs
  | otherwise
    = c:cs
