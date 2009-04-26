
module Main (main) where

import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.Simple.Build (writeAutogenFiles)
import Distribution.Simple.Register (writeInstalledConfig)
import Distribution.Text
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as Installed
         ( InstalledPackageInfo_(..) )
import qualified Distribution.Simple.PackageIndex as PackageIndex
         ( topologicalOrder, lookupPackageName, insert )

import Control.Monad
import Data.Maybe
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main = do args <- getArgs
          case args of
              "haddock" : distDir : dir : args' ->
                  runHaddock distDir dir args'
              "install" : ghcpkg : ghcpkgconfig : directory : distDir
                        : myDestDir : myPrefix : myLibdir : myDocdir : args' ->
                  doInstall ghcpkg ghcpkgconfig directory distDir
                            myDestDir myPrefix myLibdir myDocdir args'
              "configure" : args' -> case break (== "--") args' of
                   (config_args, "--" : distdir : directories) ->
                       mapM_ (generate config_args distdir) directories
                   _ -> die syntax_error
              _ -> die syntax_error

syntax_error :: [String]
syntax_error =
    ["syntax: ghc-cabal configure <configure-args> -- <distdir> <directory>...",
     "        ghc-cabal install <ghc-pkg> <directory> <distdir> <destdir> <prefix> <args>...",
     "        ghc-cabal haddock <distdir> <directory> <args>..."]

die :: [String] -> IO ()
die errs = do mapM_ (hPutStrLn stderr) errs
              exitWith (ExitFailure 1)

-- XXX Should use bracket
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory io
 = do curDirectory <- getCurrentDirectory
      setCurrentDirectory directory
      r <- io
      setCurrentDirectory curDirectory
      return r

-- We need to use the autoconfUserHooks, as the packages that use
-- configure can create a .buildinfo file, and we need any info that
-- ends up in it.
userHooks :: UserHooks
userHooks = autoconfUserHooks

runHaddock :: FilePath -> FilePath -> [String] -> IO ()
runHaddock distdir directory args
 = withCurrentDirectory directory
 $ defaultMainWithHooksArgs hooks ("haddock" : "--builddir" : distdir : args)
    where
      hooks = userHooks {
                  haddockHook = modHook (haddockHook userHooks)
              }
      modHook f pd lbi us flags
       | packageName pd == PackageName "ghc-prim"
          = let pd' = case library pd of
                      Just lib ->
                          let ghcPrim = fromJust (simpleParse "GHC.Prim")
                              ems = filter (ghcPrim /=)
                                           (exposedModules lib)
                              lib' = lib { exposedModules = ems }
                          in pd { library = Just lib' }
                      Nothing ->
                          error "Expected a library, but none found"
                pc = withPrograms lbi
                pc' = userSpecifyArgs "haddock"
                          ["dist-install/build/autogen/GHC/Prim.hs"] pc
                lbi' = lbi { withPrograms = pc' }
            in f pd' lbi' us flags
       | otherwise
          = f pd lbi us flags

doInstall :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
          -> FilePath -> FilePath -> FilePath -> [String] -> IO ()
doInstall ghcpkg ghcpkgconf directory distDir myDestDir myPrefix myLibdir myDocdir args
 = withCurrentDirectory directory $ do
     defaultMainWithHooksArgs hooks (["copy", "--builddir", distDir]
                                     ++ (if null myDestDir then []
                                           else ["--destdir", myDestDir])
                                     ++ args)
     defaultMainWithHooksArgs hooks ("register" : "--builddir" : distDir : args)
    where
      hooks = userHooks {
                  copyHook = modHook (copyHook userHooks),
                  regHook  = modHook (regHook userHooks)
              }

      modHook f pd lbi us flags
              = let
                    pd'
                     | packageName pd == PackageName "ghc-prim" =
                        case library pd of
                        Just lib ->
                            let ghcPrim = fromJust (simpleParse "GHC.Prim")
                                ems = filter (ghcPrim /=) (exposedModules lib)
                                lib' = lib { exposedModules = ems }
                            in pd { library = Just lib' }
                        Nothing ->
                            error "Expected a library, but none found"
                     | otherwise = pd
                    idts = installDirTemplates lbi
                    idts' = idts { prefix    = toPathTemplate myPrefix,
                                   libdir    = toPathTemplate myLibdir,
                                   libsubdir = toPathTemplate "$pkgid",
                                   docdir    = toPathTemplate (myDocdir </> "$pkgid"),
                                   htmldir   = toPathTemplate "$docdir" }
                    progs = withPrograms lbi
                    prog = ConfiguredProgram {
                               programId = programName ghcPkgProgram,
                               programVersion = Nothing,
                               programArgs = ["--global-conf", ghcpkgconf]
                                             ++ if not (null myDestDir)
                                                then ["--force"]
                                                else [],
                               programLocation = UserSpecified ghcpkg
                           }
                    progs' = updateProgram prog progs
                    lbi' = lbi {
                                   installDirTemplates = idts',
                                   withPrograms = progs'
                               }
                in f pd' lbi' us flags

generate :: [String] -> FilePath -> FilePath -> IO ()
generate config_args distdir directory
 = withCurrentDirectory directory
 $ do let verbosity = verbose
      gpdFile <- defaultPackageDesc verbosity
      gpd <- readPackageDescription verbosity gpdFile

      -- XXX We shouldn't just configure with the default flags
      -- XXX And this, and thus the "getPersistBuildConfig distdir" below,
      -- aren't going to work when the deps aren't built yet
      withArgs (["configure", "--distdir", distdir] ++ config_args)
          (case buildType (flattenPackageDescription gpd) of
              Just Configure -> defaultMainWithHooks autoconfUserHooks
              _other         -> defaultMain)
              -- not quite right, but good enough for us

      lbi <- getPersistBuildConfig distdir
      let pd0 = localPkgDescr lbi

      hooked_bi <-
           if (buildType pd0 == Just Configure)
           then do
              maybe_infoFile <- defaultHookedPackageDesc
              case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> readHookedBuildInfo verbosity infoFile
           else
              return emptyHookedBuildInfo

      let pd = updatePackageDescription hooked_bi pd0

      -- generate Paths_<pkg>.hs and cabal-macros.h
      writeAutogenFiles verbosity pd lbi

      -- generate inplace-pkg-config
      when (isJust $ library pd) $
          writeInstalledConfig distdir pd lbi True Nothing

      let
          libBiModules lib = (libBuildInfo lib, libModules pd)
          exeBiModules exe = (buildInfo exe, ModuleName.main : exeModules pd)
          biModuless = (maybeToList $ fmap libBiModules $ library pd)
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
            case PackageIndex.lookupPackageName index (PackageName "rts") of
              [rts] -> PackageIndex.insert rts { Installed.ldOptions = [] } index
              _ -> error "No (or multiple) ghc rts package is registered!!"

      let variablePrefix = directory ++ '_':distdir
      let xs = [variablePrefix ++ "_VERSION = " ++ display (pkgVersion (package pd)),
                variablePrefix ++ "_MODULES = " ++ unwords (map display modules),
                variablePrefix ++ "_HS_SRC_DIRS = " ++ unwords (hsSourceDirs bi),
                variablePrefix ++ "_DEPS = " ++ unwords (map display (packageDeps lbi)),
                variablePrefix ++ "_DEP_NAMES = " ++ unwords (map (display . packageName) (packageDeps lbi)),
                variablePrefix ++ "_INCLUDE_DIRS = " ++ unwords (includeDirs bi),
                variablePrefix ++ "_INCLUDES = " ++ unwords (includes bi),
                variablePrefix ++ "_INSTALL_INCLUDES = " ++ unwords (installIncludes bi),
                variablePrefix ++ "_EXTRA_LIBRARIES = " ++ unwords (extraLibs bi),
                variablePrefix ++ "_EXTRA_LIBDIRS = " ++ unwords (extraLibDirs bi),
                variablePrefix ++ "_C_SRCS  = " ++ unwords (cSources bi),
                -- XXX This includes things it shouldn't, like:
                -- -odir dist-bootstrapping/build
                variablePrefix ++ "_HC_OPTS = " ++ escape (unwords 
                        (programArgs ghcProg
                        ++ hcOptions GHC bi
                        ++ extensionsToFlags (compiler lbi) (extensions bi))),
                variablePrefix ++ "_CC_OPTS = " ++ unwords (ccOptions bi),
                variablePrefix ++ "_CPP_OPTS = " ++ unwords (cppOptions bi),
                variablePrefix ++ "_LD_OPTS = " ++ unwords (ldOptions bi),
                variablePrefix ++ "_DEP_INCLUDE_DIRS = " ++ unwords (forDeps Installed.includeDirs),
                variablePrefix ++ "_DEP_CC_OPTS = "    ++ unwords (forDeps Installed.ccOptions),
                variablePrefix ++ "_DEP_LIB_DIRS = "   ++ unwords (forDeps Installed.libraryDirs),
                variablePrefix ++ "_DEP_EXTRA_LIBS = " ++ unwords (forDeps Installed.extraLibraries),
                variablePrefix ++ "_DEP_LD_OPTS = "    ++ unwords (forDeps Installed.ldOptions)]
      writeFile (distdir ++ "/package-data.mk") $ unlines xs
  where
     escape = foldr (\c xs -> if c == '#' then '\\':'#':xs else c:xs) []
