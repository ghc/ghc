{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.HaskellSuite where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Data.Map as Map (empty)

import Distribution.Simple.Program
import Distribution.Simple.Compiler as Compiler
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text
import Distribution.Package
import Distribution.InstalledPackageInfo hiding (includeDirs)
import Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.System (Platform)
import Distribution.Compat.Exception
import Language.Haskell.Extension
import Distribution.Simple.Program.Builtin

configure
  :: Verbosity -> Maybe FilePath -> Maybe FilePath
  -> ProgramDb -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity mbHcPath hcPkgPath progdb0 = do

  -- We have no idea how a haskell-suite tool is named, so we require at
  -- least some information from the user.
  hcPath <-
    let msg = "You have to provide name or path of a haskell-suite tool (-w PATH)"
    in maybe (die' verbosity msg) return mbHcPath

  when (isJust hcPkgPath) $
    warn verbosity "--with-hc-pkg option is ignored for haskell-suite"

  (comp, confdCompiler, progdb1) <- configureCompiler hcPath progdb0

  -- Update our pkg tool. It uses the same executable as the compiler, but
  -- all command start with "pkg"
  (confdPkg, _) <- requireProgram verbosity haskellSuitePkgProgram progdb1
  let progdb2 =
        updateProgram
          confdPkg
            { programLocation = programLocation confdCompiler
            , programDefaultArgs = ["pkg"]
            }
          progdb1

  return (comp, Nothing, progdb2)

  where
    configureCompiler hcPath progdb0' = do
      let
        haskellSuiteProgram' =
          haskellSuiteProgram
            { programFindLocation = \v p -> findProgramOnSearchPath v p hcPath }

      -- NB: cannot call requireProgram right away — it'd think that
      -- the program is already configured and won't reconfigure it again.
      -- Instead, call configureProgram directly first.
      progdb1 <- configureProgram verbosity haskellSuiteProgram' progdb0'
      (confdCompiler, progdb2) <- requireProgram verbosity haskellSuiteProgram' progdb1

      extensions <- getExtensions verbosity confdCompiler
      languages  <- getLanguages  verbosity confdCompiler
      (compName, compVersion) <-
        getCompilerVersion verbosity confdCompiler

      let
        comp = Compiler {
          compilerId             = CompilerId (HaskellSuite compName) compVersion,
          compilerAbiTag         = Compiler.NoAbiTag,
          compilerCompat         = [],
          compilerLanguages      = languages,
          compilerExtensions     = extensions,
          compilerProperties     = Map.empty
        }

      return (comp, confdCompiler, progdb2)

hstoolVersion :: Verbosity -> FilePath -> IO (Maybe Version)
hstoolVersion = findProgramVersion "--hspkg-version" id

numericVersion :: Verbosity -> FilePath -> IO (Maybe Version)
numericVersion = findProgramVersion "--compiler-version" (last . words)

getCompilerVersion :: Verbosity -> ConfiguredProgram -> IO (String, Version)
getCompilerVersion verbosity prog = do
  output <- rawSystemStdout verbosity (programPath prog) ["--compiler-version"]
  let
    parts = words output
    name = concat $ init parts -- there shouldn't be any spaces in the name anyway
    versionStr = last parts
  version <-
    maybe (die' verbosity "haskell-suite: couldn't determine compiler version") return $
      simpleParse versionStr
  return (name, version)

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Compiler.Flag)]
getExtensions verbosity prog = do
  extStrs <-
    lines `fmap`
    rawSystemStdout verbosity (programPath prog) ["--supported-extensions"]
  return
    [ (ext, "-X" ++ display ext) | Just ext <- map simpleParse extStrs ]

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Compiler.Flag)]
getLanguages verbosity prog = do
  langStrs <-
    lines `fmap`
    rawSystemStdout verbosity (programPath prog) ["--supported-languages"]
  return
    [ (ext, "-G" ++ display ext) | Just ext <- map simpleParse langStrs ]

-- Other compilers do some kind of a packagedb stack check here. Not sure
-- if we need something like that as well.
getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramDb
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity packagedbs progdb =
  liftM (PackageIndex.fromList . concat) $ for packagedbs $ \packagedb ->
    do str <-
        getDbProgramOutput verbosity haskellSuitePkgProgram progdb
                ["dump", packageDbOpt packagedb]
         `catchExit` \_ -> die' verbosity $ "pkg dump failed"
       case parsePackages str of
         Right ok -> return ok
         _       -> die' verbosity "failed to parse output of 'pkg dump'"

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Right [ pkg | ParseOk _ pkg <- parsed ]
            msgs -> Left msgs

    splitPkgs :: String -> [String]
    splitPkgs = map unlines . splitWith ("---" ==) . lines
      where
        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

buildLib
  :: Verbosity -> PackageDescription -> LocalBuildInfo
  -> Library -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  -- In future, there should be a mechanism for the compiler to request any
  -- number of the above parameters (or their parts) — in particular,
  -- pieces of PackageDescription.
  --
  -- For now, we only pass those that we know are used.

  let odir = buildDir lbi
      bi = libBuildInfo lib
      srcDirs = hsSourceDirs bi ++ [odir]
      dbStack = withPackageDB lbi
      language = fromMaybe Haskell98 (defaultLanguage bi)
      progdb = withPrograms lbi
      pkgid = packageId pkg_descr

  runDbProgram verbosity haskellSuiteProgram progdb $
    [ "compile", "--build-dir", odir ] ++
    concat [ ["-i", d] | d <- srcDirs ] ++
    concat [ ["-I", d] | d <- [autogenComponentModulesDir lbi clbi
                              ,autogenPackageModulesDir lbi
                              ,odir] ++ includeDirs bi ] ++
    [ packageDbOpt pkgDb | pkgDb <- dbStack ] ++
    [ "--package-name", display pkgid ] ++
    concat [ ["--package-id", display ipkgid ]
           | (ipkgid, _) <- componentPackageDeps clbi ] ++
    ["-G", display language] ++
    concat [ ["-X", display ex] | ex <- usedExtensions bi ] ++
    cppOptions (libBuildInfo lib) ++
    [ display modu | modu <- allLibModules lib clbi ]



installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath  -- ^install location
  -> FilePath  -- ^install location for dynamic libraries
  -> FilePath  -- ^Build location
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib clbi = do
  let progdb = withPrograms lbi
  runDbProgram verbosity haskellSuitePkgProgram progdb $
    [ "install-library"
    , "--build-dir", builtDir
    , "--target-dir", targetDir
    , "--dynlib-target-dir", dynlibTargetDir
    , "--package-id", display $ packageId pkg
    ] ++ map display (allLibModules lib clbi)

registerPackage
  :: Verbosity
  -> ProgramDb
  -> PackageDBStack
  -> InstalledPackageInfo
  -> IO ()
registerPackage verbosity progdb packageDbs installedPkgInfo = do
  (hspkg, _) <- requireProgram verbosity haskellSuitePkgProgram progdb

  runProgramInvocation verbosity $
    (programInvocation hspkg
      ["update", packageDbOpt $ last packageDbs])
      { progInvokeInput = Just $ showInstalledPackageInfo installedPkgInfo }

initPackageDB :: Verbosity -> ProgramDb -> FilePath -> IO ()
initPackageDB verbosity progdb dbPath =
  runDbProgram verbosity haskellSuitePkgProgram progdb
    ["init", dbPath]

packageDbOpt :: PackageDB -> String
packageDbOpt GlobalPackageDB        = "--global"
packageDbOpt UserPackageDB          = "--user"
packageDbOpt (SpecificPackageDB db) = "--package-db=" ++ db
