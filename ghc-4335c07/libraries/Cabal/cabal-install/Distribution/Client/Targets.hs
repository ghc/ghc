{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Targets
-- Copyright   :  (c) Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified targets
-----------------------------------------------------------------------------
module Distribution.Client.Targets (
  -- * User targets
  UserTarget(..),
  readUserTargets,

  -- * Resolving user targets to package specifiers
  resolveUserTargets,

  -- ** Detailed interface
  UserTargetProblem(..),
  readUserTarget,
  reportUserTargetProblems,
  expandUserTarget,

  PackageTarget(..),
  fetchPackageTarget,
  readPackageTarget,

  PackageTargetProblem(..),
  reportPackageTargetProblems,

  disambiguatePackageTargets,
  disambiguatePackageName,

  -- * User constraints
  UserQualifier(..),
  UserConstraintScope(..),
  UserConstraint(..),
  userConstraintPackageName,
  readUserConstraint,
  userToPackageConstraint,

  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Package
         ( Package(..), PackageName, unPackageName, mkPackageName
         , PackageIdentifier(..), packageName, packageVersion )
import Distribution.Types.Dependency
import Distribution.Client.Types
         ( PackageLocation(..), ResolvedPkgLoc, UnresolvedSourcePackage
         , PackageSpecifier(..) )

import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Solver.Types.PackageConstraint
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import           Distribution.Solver.Types.SourcePackage

import qualified Distribution.Client.World as World
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.FetchUtils
import Distribution.Client.Utils ( tryFindPackageDesc )
import Distribution.Client.GlobalFlags
         ( RepoContext(..) )

import Distribution.PackageDescription
         ( GenericPackageDescription, parseFlagAssignment, nullFlagAssignment )
import Distribution.Version
         ( nullVersion, thisVersion, anyVersion, isAnyVersion )
import Distribution.Text
         ( Text(..), display )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die', warn, lowercase )

import Distribution.PackageDescription.Parsec
         ( readGenericPackageDescription, parseGenericPackageDescriptionMaybe )

-- import Data.List ( find, nub )
import Data.Either
         ( partitionEithers )
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Client.GZipUtils as GZipUtils
import Control.Monad (mapM)
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
         ( (+++), (<++) )
import Distribution.ParseUtils
         ( readPToMaybe )
import System.FilePath
         ( takeExtension, dropExtension, takeDirectory, splitPath )
import System.Directory
         ( doesFileExist, doesDirectoryExist )
import Network.URI
         ( URI(..), URIAuth(..), parseAbsoluteURI )

-- ------------------------------------------------------------
-- * User targets
-- ------------------------------------------------------------

-- | Various ways that a user may specify a package or package collection.
--
data UserTarget =

     -- | A partially specified package, identified by name and possibly with
     -- an exact version or a version constraint.
     --
     -- > cabal install foo
     -- > cabal install foo-1.0
     -- > cabal install 'foo < 2'
     --
     UserTargetNamed Dependency

     -- | A special virtual package that refers to the collection of packages
     -- recorded in the world file that the user specifically installed.
     --
     -- > cabal install world
     --
   | UserTargetWorld

     -- | A specific package that is unpacked in a local directory, often the
     -- current directory.
     --
     -- > cabal install .
     -- > cabal install ../lib/other
     --
     -- * Note: in future, if multiple @.cabal@ files are allowed in a single
     -- directory then this will refer to the collection of packages.
     --
   | UserTargetLocalDir FilePath

     -- | A specific local unpacked package, identified by its @.cabal@ file.
     --
     -- > cabal install foo.cabal
     -- > cabal install ../lib/other/bar.cabal
     --
   | UserTargetLocalCabalFile FilePath

     -- | A specific package that is available as a local tarball file
     --
     -- > cabal install dist/foo-1.0.tar.gz
     -- > cabal install ../build/baz-1.0.tar.gz
     --
   | UserTargetLocalTarball FilePath

     -- | A specific package that is available as a remote tarball file
     --
     -- > cabal install http://code.haskell.org/~user/foo/foo-0.9.tar.gz
     --
   | UserTargetRemoteTarball URI
  deriving (Show,Eq)


-- ------------------------------------------------------------
-- * Parsing and checking user targets
-- ------------------------------------------------------------

readUserTargets :: Verbosity -> [String] -> IO [UserTarget]
readUserTargets verbosity targetStrs = do
    (problems, targets) <- liftM partitionEithers
                                 (mapM readUserTarget targetStrs)
    reportUserTargetProblems verbosity problems
    return targets


data UserTargetProblem
   = UserTargetUnexpectedFile      String
   | UserTargetNonexistantFile     String
   | UserTargetUnexpectedUriScheme String
   | UserTargetUnrecognisedUri     String
   | UserTargetUnrecognised        String
   | UserTargetBadWorldPkg
  deriving Show

readUserTarget :: String -> IO (Either UserTargetProblem UserTarget)
readUserTarget targetstr =
    case testNamedTargets targetstr of
      Just (Dependency pkgn verrange)
        | pkgn == mkPackageName "world"
          -> return $ if verrange == anyVersion
                      then Right UserTargetWorld
                      else Left  UserTargetBadWorldPkg
      Just dep -> return (Right (UserTargetNamed dep))
      Nothing -> do
        fileTarget <- testFileTargets targetstr
        case fileTarget of
          Just target -> return target
          Nothing     ->
            case testUriTargets targetstr of
              Just target -> return target
              Nothing     -> return (Left (UserTargetUnrecognised targetstr))
  where
    testNamedTargets = readPToMaybe parseDependencyOrPackageId

    testFileTargets filename = do
      isDir  <- doesDirectoryExist filename
      isFile <- doesFileExist filename
      parentDirExists <- case takeDirectory filename of
                           []  -> return False
                           dir -> doesDirectoryExist dir
      let result
            | isDir
            = Just (Right (UserTargetLocalDir filename))

            | isFile && extensionIsTarGz filename
            = Just (Right (UserTargetLocalTarball filename))

            | isFile && takeExtension filename == ".cabal"
            = Just (Right (UserTargetLocalCabalFile filename))

            | isFile
            = Just (Left (UserTargetUnexpectedFile filename))

            | parentDirExists
            = Just (Left (UserTargetNonexistantFile filename))

            | otherwise
            = Nothing
      return result

    testUriTargets str =
      case parseAbsoluteURI str of
        Just uri@URI {
            uriScheme    = scheme,
            uriAuthority = Just URIAuth { uriRegName = host }
          }
          | scheme /= "http:" && scheme /= "https:" ->
            Just (Left (UserTargetUnexpectedUriScheme targetstr))

          | null host ->
            Just (Left (UserTargetUnrecognisedUri targetstr))

          | otherwise ->
            Just (Right (UserTargetRemoteTarball uri))
        _ -> Nothing

    extensionIsTarGz f = takeExtension f                 == ".gz"
                      && takeExtension (dropExtension f) == ".tar"

    parseDependencyOrPackageId :: Parse.ReadP r Dependency
    parseDependencyOrPackageId = parse
                             +++ liftM pkgidToDependency parse
      where
        pkgidToDependency :: PackageIdentifier -> Dependency
        pkgidToDependency p = case packageVersion p of
          v | v == nullVersion -> Dependency (packageName p) anyVersion
            | otherwise        -> Dependency (packageName p) (thisVersion v)


reportUserTargetProblems :: Verbosity -> [UserTargetProblem] -> IO ()
reportUserTargetProblems verbosity problems = do
    case [ target | UserTargetUnrecognised target <- problems ] of
      []     -> return ()
      target -> die' verbosity
              $ unlines
                  [ "Unrecognised target '" ++ name ++ "'."
                  | name <- target ]
             ++ "Targets can be:\n"
             ++ " - package names, e.g. 'pkgname', 'pkgname-1.0.1', 'pkgname < 2.0'\n"
             ++ " - the special 'world' target\n"
             ++ " - cabal files 'pkgname.cabal' or package directories 'pkgname/'\n"
             ++ " - package tarballs 'pkgname.tar.gz' or 'http://example.com/pkgname.tar.gz'"

    case [ () | UserTargetBadWorldPkg <- problems ] of
      [] -> return ()
      _  -> die' verbosity "The special 'world' target does not take any version."

    case [ target | UserTargetNonexistantFile target <- problems ] of
      []     -> return ()
      target -> die' verbosity
              $ unlines
                  [ "The file does not exist '" ++ name ++ "'."
                  | name <- target ]

    case [ target | UserTargetUnexpectedFile target <- problems ] of
      []     -> return ()
      target -> die' verbosity
              $ unlines
                  [ "Unrecognised file target '" ++ name ++ "'."
                  | name <- target ]
             ++ "File targets can be either package tarballs 'pkgname.tar.gz' "
             ++ "or cabal files 'pkgname.cabal'."

    case [ target | UserTargetUnexpectedUriScheme target <- problems ] of
      []     -> return ()
      target -> die' verbosity
              $ unlines
                  [ "URL target not supported '" ++ name ++ "'."
                  | name <- target ]
             ++ "Only 'http://' and 'https://' URLs are supported."

    case [ target | UserTargetUnrecognisedUri target <- problems ] of
      []     -> return ()
      target -> die' verbosity
              $ unlines
                  [ "Unrecognise URL target '" ++ name ++ "'."
                  | name <- target ]


-- ------------------------------------------------------------
-- * Resolving user targets to package specifiers
-- ------------------------------------------------------------

-- | Given a bunch of user-specified targets, try to resolve what it is they
-- refer to. They can either be specific packages (local dirs, tarballs etc)
-- or they can be named packages (with or without version info).
--
resolveUserTargets :: Package pkg
                   => Verbosity
                   -> RepoContext
                   -> FilePath
                   -> PackageIndex pkg
                   -> [UserTarget]
                   -> IO [PackageSpecifier UnresolvedSourcePackage]
resolveUserTargets verbosity repoCtxt worldFile available userTargets = do

    -- given the user targets, get a list of fully or partially resolved
    -- package references
    packageTargets <- mapM (readPackageTarget verbosity)
                  =<< mapM (fetchPackageTarget verbosity repoCtxt) . concat
                  =<< mapM (expandUserTarget verbosity worldFile) userTargets

    -- users are allowed to give package names case-insensitively, so we must
    -- disambiguate named package references
    let (problems, packageSpecifiers) =
           disambiguatePackageTargets available availableExtra packageTargets

        -- use any extra specific available packages to help us disambiguate
        availableExtra = [ packageName pkg
                         | PackageTargetLocation pkg <- packageTargets ]

    reportPackageTargetProblems verbosity problems

    return packageSpecifiers


-- ------------------------------------------------------------
-- * Package targets
-- ------------------------------------------------------------

-- | An intermediate between a 'UserTarget' and a resolved 'PackageSpecifier'.
-- Unlike a 'UserTarget', a 'PackageTarget' refers only to a single package.
--
data PackageTarget pkg =
     PackageTargetNamed      PackageName [PackageProperty] UserTarget

     -- | A package identified by name, but case insensitively, so it needs
     -- to be resolved to the right case-sensitive name.
   | PackageTargetNamedFuzzy PackageName [PackageProperty] UserTarget
   | PackageTargetLocation pkg
  deriving (Show, Functor, Foldable, Traversable)


-- ------------------------------------------------------------
-- * Converting user targets to package targets
-- ------------------------------------------------------------

-- | Given a user-specified target, expand it to a bunch of package targets
-- (each of which refers to only one package).
--
expandUserTarget :: Verbosity
                 -> FilePath
                 -> UserTarget
                 -> IO [PackageTarget (PackageLocation ())]
expandUserTarget verbosity worldFile userTarget = case userTarget of

    UserTargetNamed (Dependency name vrange) ->
      let props = [ PackagePropertyVersion vrange
                  | not (isAnyVersion vrange) ]
      in  return [PackageTargetNamedFuzzy name props userTarget]

    UserTargetWorld -> do
      worldPkgs <- World.getContents verbosity worldFile
      --TODO: should we warn if there are no world targets?
      return [ PackageTargetNamed name props userTarget
             | World.WorldPkgInfo (Dependency name vrange) flags <- worldPkgs
             , let props = [ PackagePropertyVersion vrange
                           | not (isAnyVersion vrange) ]
                        ++ [ PackagePropertyFlags flags
                           | not (nullFlagAssignment flags) ] ]

    UserTargetLocalDir dir ->
      return [ PackageTargetLocation (LocalUnpackedPackage dir) ]

    UserTargetLocalCabalFile file -> do
      let dir = takeDirectory file
      _   <- tryFindPackageDesc verbosity dir (localPackageError dir) -- just as a check
      return [ PackageTargetLocation (LocalUnpackedPackage dir) ]

    UserTargetLocalTarball tarballFile ->
      return [ PackageTargetLocation (LocalTarballPackage tarballFile) ]

    UserTargetRemoteTarball tarballURL ->
      return [ PackageTargetLocation (RemoteTarballPackage tarballURL ()) ]

localPackageError :: FilePath -> String
localPackageError dir =
    "Error reading local package.\nCouldn't find .cabal file in: " ++ dir

-- ------------------------------------------------------------
-- * Fetching and reading package targets
-- ------------------------------------------------------------


-- | Fetch any remote targets so that they can be read.
--
fetchPackageTarget :: Verbosity
                   -> RepoContext
                   -> PackageTarget (PackageLocation ())
                   -> IO (PackageTarget ResolvedPkgLoc)
fetchPackageTarget verbosity repoCtxt = traverse $
  fetchPackage verbosity repoCtxt . fmap (const Nothing)


-- | Given a package target that has been fetched, read the .cabal file.
--
-- This only affects targets given by location, named targets are unaffected.
--
readPackageTarget :: Verbosity
                  -> PackageTarget ResolvedPkgLoc
                  -> IO (PackageTarget UnresolvedSourcePackage)
readPackageTarget verbosity = traverse modifyLocation
  where
    modifyLocation location = case location of

      LocalUnpackedPackage dir -> do
        pkg <- tryFindPackageDesc verbosity dir (localPackageError dir) >>=
                 readGenericPackageDescription verbosity
        return $ SourcePackage {
                   packageInfoId        = packageId pkg,
                   packageDescription   = pkg,
                   packageSource        = fmap Just location,
                   packageDescrOverride = Nothing
                 }

      LocalTarballPackage tarballFile ->
        readTarballPackageTarget location tarballFile tarballFile

      RemoteTarballPackage tarballURL tarballFile ->
        readTarballPackageTarget location tarballFile (show tarballURL)

      RepoTarballPackage _repo _pkgid _ ->
        error "TODO: readPackageTarget RepoTarballPackage"
        -- For repo tarballs this info should be obtained from the index.

    readTarballPackageTarget location tarballFile tarballOriginalLoc = do
      (filename, content) <- extractTarballPackageCabalFile
                               tarballFile tarballOriginalLoc
      case parsePackageDescription' content of
        Nothing  -> die' verbosity $ "Could not parse the cabal file "
                       ++ filename ++ " in " ++ tarballFile
        Just pkg ->
          return $ SourcePackage {
                     packageInfoId        = packageId pkg,
                     packageDescription   = pkg,
                     packageSource        = fmap Just location,
                     packageDescrOverride = Nothing
                   }

    extractTarballPackageCabalFile :: FilePath -> String
                                   -> IO (FilePath, BS.ByteString)
    extractTarballPackageCabalFile tarballFile tarballOriginalLoc =
          either (die' verbosity . formatErr) return
        . check
        . accumEntryMap
        . Tar.filterEntries isCabalFile
        . Tar.read
        . GZipUtils.maybeDecompress
      =<< BS.readFile tarballFile
      where
        formatErr msg = "Error reading " ++ tarballOriginalLoc ++ ": " ++ msg

        accumEntryMap = Tar.foldlEntries
                          (\m e -> Map.insert (Tar.entryTarPath e) e m)
                          Map.empty

        check (Left e)  = Left (show e)
        check (Right m) = case Map.elems m of
            []     -> Left noCabalFile
            [file] -> case Tar.entryContent file of
              Tar.NormalFile content _ -> Right (Tar.entryPath file, content)
              _                        -> Left noCabalFile
            _files -> Left multipleCabalFiles
          where
            noCabalFile        = "No cabal file found"
            multipleCabalFiles = "Multiple cabal files found"

        isCabalFile e = case splitPath (Tar.entryPath e) of
          [     _dir, file] -> takeExtension file == ".cabal"
          [".", _dir, file] -> takeExtension file == ".cabal"
          _                 -> False

    parsePackageDescription' :: BS.ByteString -> Maybe GenericPackageDescription
    parsePackageDescription' bs = 
        parseGenericPackageDescriptionMaybe (BS.toStrict bs)

-- ------------------------------------------------------------
-- * Checking package targets
-- ------------------------------------------------------------

data PackageTargetProblem
   = PackageNameUnknown   PackageName               UserTarget
   | PackageNameAmbiguous PackageName [PackageName] UserTarget
  deriving Show


-- | Users are allowed to give package names case-insensitively, so we must
-- disambiguate named package references.
--
disambiguatePackageTargets :: Package pkg'
                           => PackageIndex pkg'
                           -> [PackageName]
                           -> [PackageTarget pkg]
                           -> ( [PackageTargetProblem]
                              , [PackageSpecifier pkg] )
disambiguatePackageTargets availablePkgIndex availableExtra targets =
    partitionEithers (map disambiguatePackageTarget targets)
  where
    disambiguatePackageTarget packageTarget = case packageTarget of
      PackageTargetLocation pkg -> Right (SpecificSourcePackage pkg)

      PackageTargetNamed pkgname props userTarget
        | null (PackageIndex.lookupPackageName availablePkgIndex pkgname)
                    -> Left (PackageNameUnknown pkgname userTarget)
        | otherwise -> Right (NamedPackage pkgname props)

      PackageTargetNamedFuzzy pkgname props userTarget ->
        case disambiguatePackageName packageNameEnv pkgname of
          None                 -> Left  (PackageNameUnknown
                                          pkgname userTarget)
          Ambiguous   pkgnames -> Left  (PackageNameAmbiguous
                                          pkgname pkgnames userTarget)
          Unambiguous pkgname' -> Right (NamedPackage pkgname' props)

    -- use any extra specific available packages to help us disambiguate
    packageNameEnv :: PackageNameEnv
    packageNameEnv = mappend (indexPackageNameEnv availablePkgIndex)
                             (extraPackageNameEnv availableExtra)


-- | Report problems to the user. That is, if there are any problems
-- then raise an exception.
reportPackageTargetProblems :: Verbosity
                            -> [PackageTargetProblem] -> IO ()
reportPackageTargetProblems verbosity problems = do
    case [ pkg | PackageNameUnknown pkg originalTarget <- problems
               , not (isUserTagetWorld originalTarget) ] of
      []    -> return ()
      pkgs  -> die' verbosity $ unlines
                       [ "There is no package named '" ++ display name ++ "'. "
                       | name <- pkgs ]
                  ++ "You may need to run 'cabal update' to get the latest "
                  ++ "list of available packages."

    case [ (pkg, matches) | PackageNameAmbiguous pkg matches _ <- problems ] of
      []          -> return ()
      ambiguities -> die' verbosity $ unlines
                         [    "There is no package named '" ++ display name ++ "'. "
                           ++ (if length matches > 1
                               then "However, the following package names exist: "
                               else "However, the following package name exists: ")
                           ++ intercalate ", " [ "'" ++ display m ++ "'" | m <- matches]
                           ++ "."
                         | (name, matches) <- ambiguities ]

    case [ pkg | PackageNameUnknown pkg UserTargetWorld <- problems ] of
      []   -> return ()
      pkgs -> warn verbosity $
                 "The following 'world' packages will be ignored because "
              ++ "they refer to packages that cannot be found: "
              ++ intercalate ", " (map display pkgs) ++ "\n"
              ++ "You can suppress this warning by correcting the world file."
  where
    isUserTagetWorld UserTargetWorld = True; isUserTagetWorld _ = False


-- ------------------------------------------------------------
-- * Disambiguating package names
-- ------------------------------------------------------------

data MaybeAmbiguous a = None | Unambiguous a | Ambiguous [a]

-- | Given a package name and a list of matching names, figure out
-- which one it might be referring to. If there is an exact
-- case-sensitive match then that's ok (i.e. returned via
-- 'Unambiguous'). If it matches just one package case-insensitively
-- or if it matches multiple packages case-insensitively, in that case
-- the result is 'Ambiguous'.
--
-- Note: Before cabal 2.2, when only a single package matched
--       case-insensitively it would be considered 'Unambigious'.
--
disambiguatePackageName :: PackageNameEnv
                        -> PackageName
                        -> MaybeAmbiguous PackageName
disambiguatePackageName (PackageNameEnv pkgNameLookup) name =
    case nub (pkgNameLookup name) of
      []      -> None
      names   -> case find (name==) names of
                   Just name' -> Unambiguous name'
                   Nothing    -> Ambiguous names


newtype PackageNameEnv = PackageNameEnv (PackageName -> [PackageName])

instance Monoid PackageNameEnv where
  mempty = PackageNameEnv (const [])
  mappend = (<>)

instance Semigroup PackageNameEnv where
  PackageNameEnv lookupA <> PackageNameEnv lookupB =
    PackageNameEnv (\name -> lookupA name ++ lookupB name)

indexPackageNameEnv :: PackageIndex pkg -> PackageNameEnv
indexPackageNameEnv pkgIndex = PackageNameEnv pkgNameLookup
  where
    pkgNameLookup pname =
      map fst (PackageIndex.searchByName pkgIndex $ unPackageName pname)

extraPackageNameEnv :: [PackageName] -> PackageNameEnv
extraPackageNameEnv names = PackageNameEnv pkgNameLookup
  where
    pkgNameLookup pname =
      [ pname'
      | let lname = lowercase (unPackageName pname)
      , pname' <- names
      , lowercase (unPackageName pname') == lname ]


-- ------------------------------------------------------------
-- * Package constraints
-- ------------------------------------------------------------

-- | Version of 'Qualifier' that a user may specify on the
-- command line.
data UserQualifier =
  -- | Top-level dependency.
  UserQualToplevel

  -- | Setup dependency.
  | UserQualSetup PackageName

  -- | Executable dependency.
  | UserQualExe PackageName PackageName
  deriving (Eq, Show, Generic)

instance Binary UserQualifier

-- | Version of 'ConstraintScope' that a user may specify on the
-- command line.
data UserConstraintScope =
  -- | Scope that applies to the package when it has the specified qualifier.
  UserQualified UserQualifier PackageName

  -- | Scope that applies to the package when it has a setup qualifier.
  | UserAnySetupQualifier PackageName

  -- | Scope that applies to the package when it has any qualifier.
  | UserAnyQualifier PackageName
  deriving (Eq, Show, Generic)

instance Binary UserConstraintScope

fromUserQualifier :: UserQualifier -> Qualifier
fromUserQualifier UserQualToplevel = QualToplevel
fromUserQualifier (UserQualSetup name) = QualSetup name
fromUserQualifier (UserQualExe name1 name2) = QualExe name1 name2

fromUserConstraintScope :: UserConstraintScope -> ConstraintScope
fromUserConstraintScope (UserQualified q pn) =
    ScopeQualified (fromUserQualifier q) pn
fromUserConstraintScope (UserAnySetupQualifier pn) = ScopeAnySetupQualifier pn
fromUserConstraintScope (UserAnyQualifier pn) = ScopeAnyQualifier pn

-- | Version of 'PackageConstraint' that the user can specify on
-- the command line.
data UserConstraint =
    UserConstraint UserConstraintScope PackageProperty
  deriving (Eq, Show, Generic)
           
instance Binary UserConstraint

userConstraintPackageName :: UserConstraint -> PackageName
userConstraintPackageName (UserConstraint scope _) = scopePN scope
  where
    scopePN (UserQualified _ pn) = pn
    scopePN (UserAnyQualifier pn) = pn
    scopePN (UserAnySetupQualifier pn) = pn

userToPackageConstraint :: UserConstraint -> PackageConstraint
userToPackageConstraint (UserConstraint scope prop) =
  PackageConstraint (fromUserConstraintScope scope) prop

readUserConstraint :: String -> Either String UserConstraint
readUserConstraint str =
    case readPToMaybe parse str of
      Nothing -> Left msgCannotParse
      Just c  -> Right c
  where
    msgCannotParse =
         "expected a (possibly qualified) package name followed by a " ++
         "constraint, which is either a version range, 'installed', " ++
         "'source', 'test', 'bench', or flags"

instance Text UserConstraint where
  disp (UserConstraint scope prop) =
    dispPackageConstraint $ PackageConstraint (fromUserConstraintScope scope) prop
  
  parse =
    let parseConstraintScope :: Parse.ReadP a UserConstraintScope
        parseConstraintScope =
          do
             _ <- Parse.string "any."
             pn <- parse
             return (UserAnyQualifier pn)
          +++
          do
             _ <- Parse.string "setup."
             pn <- parse
             return (UserAnySetupQualifier pn)
          +++
          do
             -- Qualified name
             pn <- parse
             (return (UserQualified UserQualToplevel pn)
              +++
              do _ <- Parse.string ":setup."
                 pn2 <- parse
                 return (UserQualified (UserQualSetup pn) pn2))

              -- -- TODO: Re-enable parsing of UserQualExe once we decide on a syntax.
              --
              -- +++
              -- do _ <- Parse.string ":"
              --    pn2 <- parse
              --    _ <- Parse.string ":exe."
              --    pn3 <- parse
              --    return (UserQualExe pn pn2, pn3)
    in do
      scope <- parseConstraintScope
                       
      -- Package property
      let keyword str x = Parse.skipSpaces1 >> Parse.string str >> return x
      prop <- ((parse >>= return . PackagePropertyVersion)
               +++
               keyword "installed" PackagePropertyInstalled
               +++
               keyword "source" PackagePropertySource
               +++
               keyword "test" (PackagePropertyStanzas [TestStanzas])
               +++
               keyword "bench" (PackagePropertyStanzas [BenchStanzas]))
              -- Note: the parser is left-biased here so that we
              -- don't get an ambiguous parse from 'installed',
              -- 'source', etc. being regarded as flags.
              <++
              (Parse.skipSpaces1 >> parseFlagAssignment
               >>= return . PackagePropertyFlags)
    
      -- Result
      return (UserConstraint scope prop)
