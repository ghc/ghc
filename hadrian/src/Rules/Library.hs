module Rules.Library (libraryRules, needLibrary, libraryTargets) where

import Hadrian.BuildPath
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type
import qualified Text.Parsec      as Parsec

import Base
import Context
import Expression hiding (way, package, stage)
import Oracles.ModuleFiles
import Packages
import Rules.Gmp
import Rules.Register
import Settings
import Target
import Utilities
import Data.Time.Clock
import Rules.Generate (generatedDependencies)
import Oracles.Flag


-- * Library 'Rules'

libraryRules :: Rules ()
libraryRules = do
    root <- buildRootRules
    root -/- "**/libHS*-*.dylib"       %> buildDynamicLib root "dylib"
    root -/- "**/libHS*-*.so"          %> buildDynamicLib root "so"
    root -/- "**/libHS*-*.dll"         %> buildDynamicLib root "dll"
    root -/- "**/*.a"                  %> buildStaticLib  root
    root -/- "**/stamp-*"              %> buildPackage root
    priority 2 $ do
        root -/- "stage*/lib/**/libHS*-*.dylib" %> registerDynamicLib root "dylib"
        root -/- "stage*/lib/**/libHS*-*.so"    %> registerDynamicLib root "so"
        root -/- "stage*/lib/**/libHS*-*.dll"   %> registerDynamicLib root "dll"
        root -/- "stage*/lib/**/*.a"            %> registerStaticLib  root
        root -/- "**/HS*-*.o"   %> buildGhciLibO root
        root -/- "**/HS*-*.p_o" %> buildGhciLibO root

-- * 'Action's for building libraries

-- | Register (with ghc-pkg) a static library ('LibA') under the given build
-- root, whose path is the second argument.
registerStaticLib :: FilePath -> FilePath -> Action ()
registerStaticLib root archivePath = do
    -- Simply need the ghc-pkg database .conf file.
    GhcPkgPath _ stage _ (LibA name _ _ w)
        <- parsePath (parseGhcPkgLibA root)
                    "<.a library (register) path parser>"
                    archivePath
    let ctx = Context stage (unsafeFindPackageByName name) w Final
    need . (:[]) =<< pkgConfFile ctx

-- | Build a static library ('LibA') under the given build root, whose path is
-- the second argument.
buildStaticLib :: FilePath -> FilePath -> Action ()
buildStaticLib root archivePath = do
    l@(BuildPath _ stage _ (LibA pkgname _ _ way))
        <- parsePath (parseBuildLibA root)
                     "<.a library (build) path parser>"
                     archivePath
    let context = libAContext l
    objs <- libraryObjects context
    removeFile archivePath
    build $ target context (Ar Pack stage) objs [archivePath]
    synopsis <- pkgSynopsis (package context)
    putSuccess $ renderLibrary
        (quote pkgname ++ " (" ++ show stage ++ ", way " ++ show way ++ ").")
        archivePath synopsis

-- | Register (with ghc-pkg) a dynamic library ('LibDyn') under the given build
-- root, with the given suffix (@.so@ or @.dylib@, @.dll@), where the complete
-- path of the registered dynamic library is given as the third argument.
registerDynamicLib :: FilePath -> String -> FilePath -> Action ()
registerDynamicLib root suffix dynlibpath = do
    -- Simply need the ghc-pkg database .conf file.
    (GhcPkgPath _ stage _ (LibDyn name _ _ w _))
        <- parsePath (parseGhcPkgLibDyn root suffix)
                            "<dyn register lib parser>"
                            dynlibpath
    let ctx = Context stage (unsafeFindPackageByName name) w Final
    need . (:[]) =<< pkgConfFile ctx

-- | Build a dynamic library ('LibDyn') under the given build root, with the
-- given suffix (@.so@ or @.dylib@, @.dll@), where the complete path of the
-- archive to build is given as the third argument.
buildDynamicLib :: FilePath -> String -> FilePath -> Action ()
buildDynamicLib root suffix dynlibpath = do
    dynlib@(BuildPath _ stage _ (LibDyn pkgname _ _  way _))
      <- parsePath (parseBuildLibDyn root suffix) "<dyn lib parser>" dynlibpath
    let context = libDynContext dynlib
    synopsis <- pkgSynopsis (package context)
    deps <- contextDependencies context
    registerPackages deps
    objs <- libraryObjects context
    build $ target context (Ghc LinkHs $ Context.stage context) objs [dynlibpath]
    putSuccess $
      renderLibrary
        (quote pkgname ++ " (" ++ show stage ++ ", way " ++ show way ++ ").")
        dynlibpath synopsis

-- | Build a "GHCi library" ('LibGhci') under the given build root, with the
-- complete path of the file to build is given as the second argument.
-- See Note [Merging object files for GHCi] in GHC.Driver.Pipeline.
buildGhciLibO :: FilePath -> FilePath -> Action ()
buildGhciLibO root ghcilibPath = do
    l@(BuildPath _ stage _ (LibGhci _ _ _ _))
        <- parsePath (parseBuildLibGhci root)
                     "<.o ghci lib (build) path parser>"
                     ghcilibPath
    let context = libGhciContext l
    objs <- allObjects context
    need objs
    build $ target context (MergeObjects stage) objs [ghcilibPath]


{-
Note [Stamp Files]
~~~~~~~~~~~~~~~~~~

A package stamp file exists to communicate that all the objects for a certain
package are built.

If you need a stamp file, then it needs all the library dependencies

The format for a stamp file is defined in `pkgStampFile`. The stamp file is named
"stamp-<way>" so if you want to build base in dynamic way then need `_build/stage1/libraries/base/stamp-dyn`

By using stamp files you can easily say you want to build a library in a certain
way by needing the stamp file for that context.

Before these stamp files existed the way to declare that all objects in a certain way
were build was by needing the .conf file for the package. Stamp files decouple this
decision from creating the .conf file which does extra stuff such as linking, copying
files etc.

-}


buildPackage :: FilePath -> FilePath -> Action ()
buildPackage root fp = do
  l@(BuildPath _ _ _ (PkgStamp _ _ _ way)) <- parsePath (parseStampPath root) "<.stamp parser>" fp
  let ctx = stampContext l
  srcs <- hsSources ctx
  gens <- interpretInContext ctx generatedDependencies

  lib_targets <- libraryTargets True ctx

  need (srcs ++ gens ++ lib_targets)

  -- Write the current time into the file so the file always changes if
  -- we restamp it because a dependency changes.
  time <- liftIO $ getCurrentTime
  liftIO $ writeFile fp (show time)
  ways <- interpretInContext ctx getLibraryWays
  let hasVanilla = elem vanilla ways
      hasDynamic = elem dynamic ways
  support <- platformSupportsSharedLibs
  when ((hasVanilla && hasDynamic) &&
        support && way == vanilla) $ do
    stamp <- (pkgStampFile (ctx { way = dynamic }))
    liftIO $ writeFile stamp (show time)



-- * Helpers

-- | Return all Haskell and non-Haskell object files for the given 'Context'.
allObjects :: Context -> Action [FilePath]
allObjects context = (++) <$> nonHsObjects context <*> hsObjects context

-- | Return all the non-Haskell object files for the given library context
-- (object files built from C, C-- and sometimes other things).
nonHsObjects :: Context -> Action [FilePath]
nonHsObjects context = do
    asmSrcs <- interpretInContext context (getContextData asmSrcs)
    asmObjs <- mapM (objectPath context) asmSrcs
    cObjs   <- cObjects context
    cxxObjs <- cxxObjects context
    jsObjs  <- jsObjects context
    cmmSrcs <- interpretInContext context (getContextData cmmSrcs)
    cmmObjs <- mapM (objectPath context) cmmSrcs
    eObjs   <- extraObjects context
    return $ asmObjs ++ cObjs ++ cxxObjs ++ cmmObjs ++ jsObjs ++ eObjs

-- | Return all the Cxx object files needed to build the given library context.
cxxObjects :: Context -> Action [FilePath]
cxxObjects context = do
    srcs <- interpretInContext context (getContextData cxxSrcs)
    mapM (objectPath context) srcs

-- | Return all the C object files needed to build the given library context.
cObjects :: Context -> Action [FilePath]
cObjects context = do
    srcs <- interpretInContext context (getContextData cSrcs)
    objs <- mapM (objectPath context) srcs
    return $ if Threaded `wayUnit` way context
        then objs
        else filter ((`notElem` ["Evac_thr", "Scav_thr"]) . takeBaseName) objs

-- | Return all the JS object files to be included in the library.
jsObjects :: Context -> Action [FilePath]
jsObjects context = do
  srcs <- interpretInContext context (getContextData jsSrcs)
  mapM (objectPath context) srcs

-- | Return extra object files needed to build the given library context. The
-- resulting list is currently non-empty only when the package from the
-- 'Context' is @ghc-internal@ built with in-tree GMP backend.
extraObjects :: Context -> Action [FilePath]
extraObjects context
    | package context == ghcInternal = do
         interpretInContext context getBignumBackend >>= \case
            "gmp" -> gmpObjects (stage context)
            _     -> return []

    | otherwise = return []

-- | Return all the object files to be put into the library we're building for
-- the given 'Context'.
libraryObjects :: Context -> Action [FilePath]
libraryObjects context = do
    hsObjs   <- hsObjects    context
    noHsObjs <- nonHsObjects context
    need $ noHsObjs ++ hsObjs
    return (noHsObjs ++ hsObjs)

-- | Coarse-grain 'need': make sure all given libraries are fully built.
needLibrary :: [Context] -> Action ()
needLibrary cs = need =<< concatMapM (libraryTargets True) cs

-- * Library paths types and parsers

-- | > libHS<pkg name>-<pkg version>-<pkg hash>[_<way suffix>].a
data LibA = LibA String [Integer] String Way deriving (Eq, Show)

-- | > <so or dylib>
data DynLibExt = So | Dylib deriving (Eq, Show)

-- | > libHS<pkg name>-<pkg version>-<pkg hash>[_<way suffix>]-ghc<ghc version>.<so|dylib>
data LibDyn = LibDyn String [Integer] String Way DynLibExt deriving (Eq, Show)

-- | > HS<pkg name>-<pkg version>-<pkg hash>[_<way suffix>].o
data LibGhci = LibGhci String [Integer] String Way deriving (Eq, Show)

-- | Get the 'Context' corresponding to the build path for a given static library.
libAContext :: BuildPath LibA -> Context
libAContext (BuildPath _ stage pkgpath (LibA pkgname _ _ way)) =
    Context stage pkg way Final
  where
    pkg = library pkgname pkgpath

-- | Get the 'Context' corresponding to the build path for a given GHCi library.
libGhciContext :: BuildPath LibGhci -> Context
libGhciContext (BuildPath _ stage pkgpath (LibGhci pkgname _ _ way)) =
    Context stage pkg way Final
  where
    pkg = library pkgname pkgpath

-- | Get the 'Context' corresponding to the build path for a given dynamic library.
libDynContext :: BuildPath LibDyn -> Context
libDynContext (BuildPath _ stage pkgpath (LibDyn pkgname _ _ way _)) =
    Context stage pkg way Final
  where
    pkg = library pkgname pkgpath

-- | Get the 'Context' corresponding to the build path for a given static library.
stampContext :: BuildPath PkgStamp -> Context
stampContext (BuildPath _ stage _ (PkgStamp pkgname _ _ way)) =
    Context stage pkg way Final
  where
    pkg = unsafeFindPackageByName pkgname

data PkgStamp = PkgStamp String [Integer] String Way deriving (Eq, Show)


-- | Parse a path to a ghci library to be built, making sure the path starts
-- with the given build root.
parseStampPath :: FilePath -> Parsec.Parsec String () (BuildPath PkgStamp)
parseStampPath root = parseBuildPath root parseStamp


-- | Parse a path to a registered ghc-pkg static library to be built, making
-- sure the path starts with the given build root.
parseGhcPkgLibA :: FilePath -> Parsec.Parsec String () (GhcPkgPath LibA)
parseGhcPkgLibA root
    = parseGhcPkgPath root
        (do -- Skip past pkgId directory.
            _ <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string "/")
            parseLibAFilename)
        Parsec.<?> "ghc-pkg path for a static library"

-- | Parse a path to a static library to be built, making sure the path starts
-- with the given build root.
parseBuildLibA :: FilePath -> Parsec.Parsec String () (BuildPath LibA)
parseBuildLibA root = parseBuildPath root parseLibAFilename
    Parsec.<?> "build path for a static library"

-- | Parse a path to a ghci library to be built, making sure the path starts
-- with the given build root.
parseBuildLibGhci :: FilePath -> Parsec.Parsec String () (BuildPath LibGhci)
parseBuildLibGhci root = parseBuildPath root parseLibGhciFilename
    Parsec.<?> "build path for a ghci library"

-- | Parse a path to a dynamic library to be built, making sure the path starts
-- with the given build root.
parseBuildLibDyn :: FilePath -> String -> Parsec.Parsec String () (BuildPath LibDyn)
parseBuildLibDyn root ext = parseBuildPath root (parseLibDynFilename ext)
    Parsec.<?> ("build path for a dynamic library with extension " ++ ext)

-- | Parse a path to a registered ghc-pkg dynamic library, making sure the path
-- starts with the given package database root.
parseGhcPkgLibDyn :: FilePath -> String -> Parsec.Parsec String () (GhcPkgPath LibDyn)
parseGhcPkgLibDyn root ext = parseGhcPkgPath root (parseLibDynFilename ext)
    Parsec.<?> ("ghc-pkg path for a dynamic library with extension " ++ ext)

-- | Parse the filename of a static library to be built into a 'LibA' value.
parseLibAFilename :: Parsec.Parsec String () LibA
parseLibAFilename = do
    _ <- Parsec.string "libHS"
    (pkgname, pkgver, pkghash) <- parsePkgId
    way <- parseWaySuffix vanilla
    _ <- Parsec.string ".a"
    return (LibA pkgname pkgver pkghash way)

-- | Parse the filename of a ghci library to be built into a 'LibGhci' value.
parseLibGhciFilename :: Parsec.Parsec String () LibGhci
parseLibGhciFilename = do
    _ <- Parsec.string "HS"
    (pkgname, pkgver, pkghash) <- parsePkgId
    _ <- Parsec.string "."
    way <- parseWayPrefix vanilla
    _ <- Parsec.string "o"
    return (LibGhci pkgname pkgver pkghash way)

-- | Parse the filename of a dynamic library to be built into a 'LibDyn' value.
parseLibDynFilename :: String -> Parsec.Parsec String () LibDyn
parseLibDynFilename ext = do
    _ <- Parsec.string "libHS"
    (pkgname, pkgver, pkghash) <- parsePkgId
    way <- addWayUnit Dynamic <$> parseWaySuffix dynamic
    _ <- optional $ Parsec.string "-ghc" *> parsePkgVersion
    _ <- Parsec.string ("." ++ ext)
    return (LibDyn pkgname pkgver pkghash way $ if ext == "so" then So else Dylib)

parseStamp :: Parsec.Parsec String () PkgStamp
parseStamp = do
    _ <- Parsec.string "stamp-"
    (pkgname, pkgver, pkghash) <- parsePkgId
    way <- parseWaySuffix vanilla
    return (PkgStamp pkgname pkgver pkghash way)
