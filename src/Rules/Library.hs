module Rules.Library (libraryRules) where

import Data.Functor
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.PackageData as PD
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
    synopsis <- pkgSynopsis context
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
    cmmSrcs <- interpretInContext context (getPackageData PD.cmmSrcs)
    cmmObjs <- mapM (objectPath context) cmmSrcs
    eObjs   <- extraObjects context
    return $ cObjs ++ cmmObjs ++ eObjs

-- | Return all the C object files needed to build the given library context.
cObjects :: Context -> Action [FilePath]
cObjects context = do
    srcs <- interpretInContext context (getPackageData PD.cSrcs)
    objs <- mapM (objectPath context) srcs
    return $ if way context == threaded
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

-- | > libHS<pkg name>-<pkg version>-ghc<ghc version>[_<way suffix>].<so or dylib>
data LibDyn = LibDyn String [Integer] Way DynLibExt deriving (Eq, Show)

-- | > HS<pkg name>-<pkg version>[_<way suffix>].o
data LibGhci = LibGhci String [Integer] Way deriving (Eq, Show)

-- | A path of the form
--
-- > <build root>/stage<N>/<path/to/pkg/from/ghc/root>/build/<something>
--
-- where @something@ describes a library to be build for the given package.
--
-- @a@, which represents that @something@, is instantiated as 'LibA', 'LibDyn'
-- and 'LibGhci' successively in this module, depending on the type of library
-- we're giving the build rules for.
data BuildPath a = BuildPath FilePath -- ^ > <build root>/
                             Stage    -- ^ > stage<N>/
                             FilePath -- ^ > <path/to/pkg/from/ghc/root>/build/
                             a        -- ^ > whatever comes after 'build/'
    deriving (Eq, Show)

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

-- | Parse a build path for a library to be built under the given build root,
-- where the filename will be parsed with the given parser argument.
parseBuildPath
    :: FilePath -- ^ build root
    -> Parsec.Parsec String () a -- ^ what to parse after @build/@
    -> Parsec.Parsec String () (BuildPath a)
parseBuildPath root afterBuild = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgpath <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string "/build/")
    a <- afterBuild
    return (BuildPath root stage pkgpath a)

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
    _ <- optional $ Parsec.string "-ghc" *> parsePkgVersion
    way <- addWayUnit Dynamic <$> parseWaySuffix dynamic
    _ <- Parsec.string ("." ++ ext)
    return (LibDyn pkgname pkgver way $ if ext == "so" then So else Dylib)

-- To be kept in sync with Stage.hs's stageString function
-- | Parse @"stageX"@ into a 'Stage'.
parseStage :: Parsec.Parsec String () Stage
parseStage = (Parsec.string "stage" *> Parsec.choice
    [ Parsec.string (show n) $> toEnum n
    | n <- map fromEnum [minBound .. maxBound :: Stage]
    ]) Parsec.<?> "stage string"

-- To be kept in sync with the show instances in 'Way.Type', until we perhaps
-- use some bidirectional parsing/pretty printing approach or library.
-- | Parse a way suffix, returning the argument when no suffix is found (the
-- argument will be vanilla in most cases, but dynamic when we parse the way
-- suffix out of a shared library file name).
parseWaySuffix :: Way -> Parsec.Parsec String () Way
parseWaySuffix w = Parsec.choice
    [ Parsec.string "_" *> (wayFromUnits <$> Parsec.sepBy1 parseWayUnit (Parsec.string "_"))
    , pure w
    ] Parsec.<?> "way suffix (e.g _thr_p, or none for vanilla)"
  where
    parseWayUnit = Parsec.choice
        [ Parsec.string "thr" *> pure Threaded
        , Parsec.char   'd'   *>
          (Parsec.choice [ Parsec.string "ebug" *> pure Debug
                         , Parsec.string "yn"   *> pure Dynamic ])
        , Parsec.char 'p'     *> pure Profiling
        , Parsec.char 'l'     *> pure Logging
        ] Parsec.<?> "way unit (thr, debug, dyn, p, l)"

-- | Parse a @"pkgname-pkgversion"@ string into the package name and the
-- integers that make up the package version.
parsePkgId :: Parsec.Parsec String () (String, [Integer])
parsePkgId = parsePkgId' "" Parsec.<?> "package identifier (<name>-<version>)"
  where
    parsePkgId' currName = do
        s <- Parsec.many1 Parsec.alphaNum
        _ <- Parsec.char '-'
        let newName = if null currName then s else currName ++ "-" ++ s
        Parsec.choice [ (newName,) <$> parsePkgVersion
                      , parsePkgId' newName ]

-- | Parse "."-separated integers that describe a package's version.
parsePkgVersion :: Parsec.Parsec String () [Integer]
parsePkgVersion = fmap reverse (parsePkgVersion' []) Parsec.<?> "package version"
  where
    parsePkgVersion' xs = do
        n <- parseNatural
        Parsec.choice
            [ Parsec.try (Parsec.lookAhead (Parsec.char '.' *> (Parsec.letter <|> Parsec.char '_')))
              $> (n:xs)
            , Parsec.char '.' *> parsePkgVersion' (n:xs)
            , pure $ (n:xs) ]

-- | Parse a natural number.
parseNatural :: Parsec.Parsec String () Integer
parseNatural = (read <$> Parsec.many1 Parsec.digit) Parsec.<?> "natural number"

-- | Runs the given parser against the given path, erroring out when the parser
-- fails (because it shouldn't if the code from this module is correct).
parsePath
    :: Parsec.Parsec String () a -- ^ parser to run
    -> String                    -- ^ string describing the input source
    -> FilePath                  -- ^ path to parse
    -> Action a
parsePath p inp path = case Parsec.parse p inp path of
    Left err -> fail $ "Rules.Library.parsePath: path="
                    ++ path ++ ", error:\n" ++ show err
    Right a  -> pure a
