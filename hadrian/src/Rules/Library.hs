module Rules.Library (libraryRules) where

import Hadrian.BuildPath
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type
import qualified System.Directory as IO
import qualified Text.Parsec      as Parsec

import Base
import Context
import Expression hiding (way, package)
import Flavour
import Oracles.ModuleFiles
import Packages
import Rules.Gmp
import Settings
import Target
import Utilities

-- * Library 'Rules'

libraryRules :: Rules ()
libraryRules = do
    root <- buildRootRules
    root -/- "//libHS*-*.dylib"       %> buildDynamicLibUnix root "dylib"
    root -/- "//libHS*-*.so"          %> buildDynamicLibUnix root "so"
    root -/- "//*.a"                  %> buildStaticLib      root
    priority 2 $ root -/- "//HS*-*.o" %> buildGhciLibO       root

-- * 'Action's for building libraries

-- | Build a static library ('LibA') under the given build root, whose path is
-- the second argument.
buildStaticLib :: FilePath -> FilePath -> Action ()
buildStaticLib root archivePath = do
    l@(BuildPath _ stage _ (LibA pkgname _ way))
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

-- | Build a dynamic library ('LibDyn') under the given build root, with the
-- given suffix (@.so@ or @.dylib@, @.dll@ in the future), where the complete
-- path of the archive to build is given as the third argument.
buildDynamicLibUnix :: FilePath -> String -> FilePath -> Action ()
buildDynamicLibUnix root suffix dynlibpath = do
    dynlib <- parsePath (parseBuildLibDyn root suffix) "<dyn lib parser>" dynlibpath
    let context = libDynContext dynlib
    deps <- contextDependencies context
    need =<< mapM pkgLibraryFile deps
    objs <- libraryObjects context
    build $ target context (Ghc LinkHs $ Context.stage context) objs [dynlibpath]

-- | Build a "GHCi library" ('LibGhci') under the given build root, with the
-- complete path of the file to build is given as the second argument.
buildGhciLibO :: FilePath -> FilePath -> Action ()
buildGhciLibO root ghcilibPath = do
    l@(BuildPath _ stage _ (LibGhci _ _ _))
        <- parsePath (parseBuildLibGhci root)
                     "<.o ghci lib (build) path parser>"
                     ghcilibPath
    let context = libGhciContext l
    objs <- allObjects context
    need objs
    build $ target context (Ld stage) objs [ghcilibPath]

-- * Helpers

-- | Return all Haskell and non-Haskell object files for the given 'Context'.
allObjects :: Context -> Action [FilePath]
allObjects context = (++) <$> nonHsObjects context <*> hsObjects context

-- | Return all the non-Haskell object files for the given library context
-- (object files built from C, C-- and sometimes other things).
nonHsObjects :: Context -> Action [FilePath]
nonHsObjects context = do
    cObjs   <- cObjects context
    cmmSrcs <- interpretInContext context (getContextData cmmSrcs)
    cmmObjs <- mapM (objectPath context) cmmSrcs
    eObjs   <- extraObjects context
    return $ cObjs ++ cmmObjs ++ eObjs

-- | Return all the C object files needed to build the given library context.
cObjects :: Context -> Action [FilePath]
cObjects context = do
    srcs <- interpretInContext context (getContextData cSrcs)
    objs <- mapM (objectPath context) srcs
    return $ if Threaded `wayUnit` way context
        then objs
        else filter ((`notElem` ["Evac_thr", "Scav_thr"]) . takeBaseName) objs

-- | Return extra object files needed to build the given library context. The
-- resulting list is currently non-empty only when the package from the
-- 'Context' is @integer-gmp@.
extraObjects :: Context -> Action [FilePath]
extraObjects context
    | package context == integerGmp = do
        gmpPath <- gmpBuildPath
        need [gmpPath -/- gmpLibraryH]
        map unifyPath <$> getDirectoryFiles "" [gmpPath -/- gmpObjectsDir -/- "*.o"]
    | otherwise         = return []

-- | Return all the object files to be put into the library we're building for
-- the given 'Context'.
libraryObjects :: Context -> Action [FilePath]
libraryObjects context@Context{..} = do
    hsObjs   <- hsObjects    context
    noHsObjs <- nonHsObjects context

    -- This will create split objects if required (we don't track them
    -- explicitly as this would needlessly bloat the Shake database).
    need $ noHsObjs ++ hsObjs

    split <- interpretInContext context =<< splitObjects <$> flavour
    let getSplitObjs = concatForM hsObjs $ \obj -> do
            let dir = dropExtension obj ++ "_" ++ osuf way ++ "_split"
            contents <- liftIO $ IO.getDirectoryContents dir
            return . map (dir -/-) $ filter (not . all (== '.')) contents

    (noHsObjs ++) <$> if split then getSplitObjs else return hsObjs

-- * Library paths types and parsers

-- | > libHS<pkg name>-<pkg version>[_<way suffix>].a
data LibA = LibA String [Integer] Way deriving (Eq, Show)

-- | > <so or dylib>
data DynLibExt = So | Dylib deriving (Eq, Show)

-- | > libHS<pkg name>-<pkg version>[_<way suffix>]-ghc<ghc version>.<so|dylib>
data LibDyn = LibDyn String [Integer] Way DynLibExt deriving (Eq, Show)

-- | > HS<pkg name>-<pkg version>[_<way suffix>].o
data LibGhci = LibGhci String [Integer] Way deriving (Eq, Show)

-- | Get the 'Context' corresponding to the build path for a given static library.
libAContext :: BuildPath LibA -> Context
libAContext (BuildPath _ stage pkgpath (LibA pkgname _ way)) =
    Context stage pkg way
  where
    pkg = library pkgname pkgpath

-- | Get the 'Context' corresponding to the build path for a given GHCi library.
libGhciContext :: BuildPath LibGhci -> Context
libGhciContext (BuildPath _ stage pkgpath (LibGhci pkgname _ way)) =
    Context stage pkg way
  where
    pkg = library pkgname pkgpath

-- | Get the 'Context' corresponding to the build path for a given dynamic library.
libDynContext :: BuildPath LibDyn -> Context
libDynContext (BuildPath _ stage pkgpath (LibDyn pkgname _ way _)) =
    Context stage pkg way
  where
    pkg = library pkgname pkgpath

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

-- | Parse the filename of a static library to be built into a 'LibA' value.
parseLibAFilename :: Parsec.Parsec String () LibA
parseLibAFilename = do
    _ <- Parsec.string "libHS"
    (pkgname, pkgver) <- parsePkgId
    way <- parseWaySuffix vanilla
    _ <- Parsec.string ".a"
    return (LibA pkgname pkgver way)

-- | Parse the filename of a ghci library to be built into a 'LibGhci' value.
parseLibGhciFilename :: Parsec.Parsec String () LibGhci
parseLibGhciFilename = do
    _ <- Parsec.string "HS"
    (pkgname, pkgver) <- parsePkgId
    way <- parseWaySuffix vanilla
    _ <- Parsec.string ".o"
    return (LibGhci pkgname pkgver way)

-- | Parse the filename of a dynamic library to be built into a 'LibDyn' value.
parseLibDynFilename :: String -> Parsec.Parsec String () LibDyn
parseLibDynFilename ext = do
    _ <- Parsec.string "libHS"
    (pkgname, pkgver) <- parsePkgId
    way <- addWayUnit Dynamic <$> parseWaySuffix dynamic
    _ <- optional $ Parsec.string "-ghc" *> parsePkgVersion
    _ <- Parsec.string ("." ++ ext)
    return (LibDyn pkgname pkgver way $ if ext == "so" then So else Dylib)
