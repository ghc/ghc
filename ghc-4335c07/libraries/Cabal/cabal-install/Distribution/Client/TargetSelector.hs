{-# LANGUAGE CPP, DeriveGeneric, DeriveFunctor, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.TargetSelector
-- Copyright   :  (c) Duncan Coutts 2012, 2015, 2016
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified target selectors.
--
-----------------------------------------------------------------------------
module Distribution.Client.TargetSelector (

    -- * Target selectors
    TargetSelector(..),
    TargetImplicitCwd(..),
    ComponentKind(..),
    ComponentKindFilter,
    SubComponentTarget(..),
    QualLevel(..),
    componentKind,

    -- * Reading target selectors
    readTargetSelectors,
    TargetSelectorProblem(..),
    reportTargetSelectorProblems,
    showTargetSelector,
    TargetString,
    showTargetString,
    parseTargetString,
    -- ** non-IO
    readTargetSelectorsWith,
    DirActions(..),
    defaultDirActions,
  ) where

import Distribution.Package
         ( Package(..), PackageId, PackageIdentifier(..)
         , PackageName, packageName, mkPackageName )
import Distribution.Version
         ( mkVersion )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.Client.Types
         ( PackageLocation(..), PackageSpecifier(..) )

import Distribution.Verbosity
import Distribution.PackageDescription
         ( PackageDescription
         , Executable(..)
         , TestSuite(..), TestSuiteInterface(..), testModules
         , Benchmark(..), BenchmarkInterface(..), benchmarkModules
         , BuildInfo(..), explicitLibModules, exeModules )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Solver.Types.SourcePackage
         ( SourcePackage(..) )
import Distribution.ModuleName
         ( ModuleName, toFilePath )
import Distribution.Simple.LocalBuildInfo
         ( Component(..), ComponentName(..)
         , pkgComponents, componentName, componentBuildInfo )
import Distribution.Types.ForeignLib

import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( die', lowercase, ordNub )
import Distribution.Client.Utils
         ( makeRelativeCanonical )

import Data.Either
         ( partitionEithers )
import Data.Function
         ( on )
import Data.List
         ( nubBy, stripPrefix, partition, intercalate, sortBy, groupBy )
import Data.Maybe
         ( maybeToList )
import Data.Ord
         ( comparing )
import Distribution.Compat.Binary (Binary)
import GHC.Generics (Generic)
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Lazy   as Map.Lazy
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
#else
import qualified Data.Map as Map.Lazy
import qualified Data.Map as Map
import Data.Map (Map)
#endif
import qualified Data.Set as Set
import Control.Arrow ((&&&))
import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Applicative (Alternative(..))
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
         ( (+++), (<++) )
import Distribution.ParseUtils
         ( readPToMaybe )
import Data.Char
         ( isSpace, isAlphaNum )
import System.FilePath as FilePath
         ( takeExtension, dropExtension
         , splitDirectories, joinPath, splitPath )
import qualified System.Directory as IO
         ( doesFileExist, doesDirectoryExist, canonicalizePath
         , getCurrentDirectory )
import System.FilePath
         ( (</>), (<.>), normalise, dropTrailingPathSeparator )
import Text.EditDistance
         ( defaultEditCosts, restrictedDamerauLevenshteinDistance )


-- ------------------------------------------------------------
-- * Target selector terms
-- ------------------------------------------------------------

-- | A target selector is expression selecting a set of components (as targets
-- for a actions like @build@, @run@, @test@ etc). A target selector
-- corresponds to the user syntax for referring to targets on the command line.
--
-- From the users point of view a target can be many things: packages, dirs,
-- component names, files etc. Internally we consider a target to be a specific
-- component (or module\/file within a component), and all the users' notions
-- of targets are just different ways of referring to these component targets.
--
-- So target selectors are expressions in the sense that they are interpreted
-- to refer to one or more components. For example a 'TargetPackage' gets
-- interpreted differently by different commands to refer to all or a subset
-- of components within the package.
--
-- The syntax has lots of optional parts:
--
-- > [ package name | package dir | package .cabal file ]
-- > [ [lib:|exe:] component name ]
-- > [ module name | source file ]
--
data TargetSelector pkg =

     -- | A package as a whole: the default components for the package or all
     -- components of a particular kind.
     --
     TargetPackage TargetImplicitCwd pkg (Maybe ComponentKindFilter)

     -- | All packages, or all components of a particular kind in all packages.
     --
   | TargetAllPackages (Maybe ComponentKindFilter)

     -- | A specific component in a package.
     --
   | TargetComponent pkg ComponentName SubComponentTarget

     -- | A named package, but not a known local package. It could for example
     -- resolve to a dependency of a local package or to a package from
     -- hackage. Either way, it requires further processing to resolve.
     --
   | TargetPackageName PackageName
  deriving (Eq, Ord, Functor, Show, Generic)

-- | Does this 'TargetPackage' selector arise from syntax referring to a
-- package in the current directory (e.g. @tests@ or no giving no explicit
-- target at all) or does it come from syntax referring to a package name
-- or location.
--
data TargetImplicitCwd = TargetImplicitCwd | TargetExplicitNamed
  deriving (Eq, Ord, Show, Generic)

data ComponentKind = LibKind | FLibKind | ExeKind | TestKind | BenchKind
  deriving (Eq, Ord, Enum, Show)

type ComponentKindFilter = ComponentKind

-- | Either the component as a whole or detail about a file or module target
-- within a component.
--
data SubComponentTarget =

     -- | The component as a whole
     WholeComponent

     -- | A specific module within a component.
   | ModuleTarget ModuleName

     -- | A specific file within a component.
   | FileTarget   FilePath
  deriving (Eq, Ord, Show, Generic)

instance Binary SubComponentTarget


-- ------------------------------------------------------------
-- * Top level, do everything
-- ------------------------------------------------------------


-- | Parse a bunch of command line args as 'TargetSelector's, failing with an
-- error if any are unrecognised. The possible target selectors are based on
-- the available packages (and their locations).
--
readTargetSelectors :: [PackageSpecifier (SourcePackage (PackageLocation a))]
                    -> [String]
                    -> IO (Either [TargetSelectorProblem]
                                  [TargetSelector PackageId])
readTargetSelectors = readTargetSelectorsWith defaultDirActions

readTargetSelectorsWith :: (Applicative m, Monad m) => DirActions m
                        -> [PackageSpecifier (SourcePackage (PackageLocation a))]
                        -> [String]
                        -> m (Either [TargetSelectorProblem]
                                     [TargetSelector PackageId])
readTargetSelectorsWith dirActions@DirActions{..} pkgs targetStrs =
    case parseTargetStrings targetStrs of
      ([], utargets) -> do
        utargets' <- mapM (getTargetStringFileStatus dirActions) utargets
        pkgs'     <- sequence [ selectPackageInfo dirActions pkg
                              | SpecificSourcePackage pkg <- pkgs ]
        cwd       <- getCurrentDirectory
        let (cwdPkg, otherPkgs) = selectCwdPackage cwd pkgs'
        case resolveTargetSelectors cwdPkg otherPkgs utargets' of
          ([], btargets) -> return (Right (map (fmap packageId) btargets))
          (problems, _)  -> return (Left problems)
      (strs, _)          -> return (Left (map TargetSelectorUnrecognised strs))
  where
    selectCwdPackage :: FilePath
                     -> [PackageInfo]
                     -> ([PackageInfo], [PackageInfo])
    selectCwdPackage cwd pkgs' =
        let (cwdpkg, others) = partition isPkgDirCwd pkgs'
         in (cwdpkg, others)
      where
        isPkgDirCwd PackageInfo { pinfoDirectory = Just (dir,_) }
          | dir == cwd = True
        isPkgDirCwd _  = False

data DirActions m = DirActions {
       doesFileExist       :: FilePath -> m Bool,
       doesDirectoryExist  :: FilePath -> m Bool,
       canonicalizePath    :: FilePath -> m FilePath,
       getCurrentDirectory :: m FilePath
     }

defaultDirActions :: DirActions IO
defaultDirActions =
    DirActions {
      doesFileExist       = IO.doesFileExist,
      doesDirectoryExist  = IO.doesDirectoryExist,
      -- Workaround for <https://github.com/haskell/directory/issues/63>
      canonicalizePath    = IO.canonicalizePath . dropTrailingPathSeparator,
      getCurrentDirectory = IO.getCurrentDirectory
    }

makeRelativeToCwd :: Applicative m => DirActions m -> FilePath -> m FilePath
makeRelativeToCwd DirActions{..} path =
    makeRelativeCanonical <$> canonicalizePath path <*> getCurrentDirectory


-- ------------------------------------------------------------
-- * Parsing target strings
-- ------------------------------------------------------------

-- | The outline parse of a target selector. It takes one of the forms:
--
-- > str1
-- > str1:str2
-- > str1:str2:str3
-- > str1:str2:str3:str4
--
data TargetString =
     TargetString1 String
   | TargetString2 String String
   | TargetString3 String String String
   | TargetString4 String String String String
   | TargetString5 String String String String String
   | TargetString7 String String String String String String String
  deriving (Show, Eq)

-- | Parse a bunch of 'TargetString's (purely without throwing exceptions).
--
parseTargetStrings :: [String] -> ([String], [TargetString])
parseTargetStrings =
    partitionEithers
  . map (\str -> maybe (Left str) Right (parseTargetString str))

parseTargetString :: String -> Maybe TargetString
parseTargetString =
    readPToMaybe parseTargetApprox
  where
    parseTargetApprox :: Parse.ReadP r TargetString
    parseTargetApprox =
          (do a <- tokenQ
              return (TargetString1 a))
      +++ (do a <- tokenQ0
              _ <- Parse.char ':'
              b <- tokenQ
              return (TargetString2 a b))
      +++ (do a <- tokenQ0
              _ <- Parse.char ':'
              b <- tokenQ
              _ <- Parse.char ':'
              c <- tokenQ
              return (TargetString3 a b c))
      +++ (do a <- tokenQ0
              _ <- Parse.char ':'
              b <- token
              _ <- Parse.char ':'
              c <- tokenQ
              _ <- Parse.char ':'
              d <- tokenQ
              return (TargetString4 a b c d))
      +++ (do a <- tokenQ0
              _ <- Parse.char ':'
              b <- token
              _ <- Parse.char ':'
              c <- tokenQ
              _ <- Parse.char ':'
              d <- tokenQ
              _ <- Parse.char ':'
              e <- tokenQ
              return (TargetString5 a b c d e))
      +++ (do a <- tokenQ0
              _ <- Parse.char ':'
              b <- token
              _ <- Parse.char ':'
              c <- tokenQ
              _ <- Parse.char ':'
              d <- tokenQ
              _ <- Parse.char ':'
              e <- tokenQ
              _ <- Parse.char ':'
              f <- tokenQ
              _ <- Parse.char ':'
              g <- tokenQ
              return (TargetString7 a b c d e f g))

    token  = Parse.munch1 (\x -> not (isSpace x) && x /= ':')
    tokenQ = parseHaskellString <++ token
    token0 = Parse.munch (\x -> not (isSpace x) && x /= ':')
    tokenQ0= parseHaskellString <++ token0
    parseHaskellString :: Parse.ReadP r String
    parseHaskellString = Parse.readS_to_P reads


-- | Render a 'TargetString' back as the external syntax. This is mainly for
-- error messages.
--
showTargetString :: TargetString -> String
showTargetString = intercalate ":" . components
  where
    components (TargetString1 s1)          = [s1]
    components (TargetString2 s1 s2)       = [s1,s2]
    components (TargetString3 s1 s2 s3)    = [s1,s2,s3]
    components (TargetString4 s1 s2 s3 s4) = [s1,s2,s3,s4]
    components (TargetString5 s1 s2 s3 s4 s5)       = [s1,s2,s3,s4,s5]
    components (TargetString7 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]

showTargetSelector :: Package p => TargetSelector p -> String
showTargetSelector ts =
  case [ t | ql <- [QL1 .. QLFull]
           , t  <- renderTargetSelector ql ts ]
  of (t':_) -> showTargetString (forgetFileStatus t')
     [] -> ""

showTargetSelectorKind :: TargetSelector a -> String
showTargetSelectorKind bt = case bt of
  TargetPackage TargetExplicitNamed _ Nothing  -> "package"
  TargetPackage TargetExplicitNamed _ (Just _) -> "package:filter"
  TargetPackage TargetImplicitCwd   _ Nothing  -> "cwd-package"
  TargetPackage TargetImplicitCwd   _ (Just _) -> "cwd-package:filter"
  TargetAllPackages Nothing                    -> "all-packages"
  TargetAllPackages (Just _)                   -> "all-packages:filter"
  TargetComponent _ _ WholeComponent           -> "component"
  TargetComponent _ _ ModuleTarget{}           -> "module"
  TargetComponent _ _ FileTarget{}             -> "file"
  TargetPackageName{}                          -> "package name"


-- ------------------------------------------------------------
-- * Checking if targets exist as files
-- ------------------------------------------------------------

data TargetStringFileStatus =
     TargetStringFileStatus1 String FileStatus
   | TargetStringFileStatus2 String FileStatus String
   | TargetStringFileStatus3 String FileStatus String String
   | TargetStringFileStatus4 String String String String
   | TargetStringFileStatus5 String String String String String
   | TargetStringFileStatus7 String String String String String String String
  deriving (Eq, Ord, Show)

data FileStatus = FileStatusExistsFile FilePath -- the canonicalised filepath
                | FileStatusExistsDir  FilePath -- the canonicalised filepath
                | FileStatusNotExists  Bool -- does the parent dir exist even?
  deriving (Eq, Ord, Show)

noFileStatus :: FileStatus
noFileStatus = FileStatusNotExists False

getTargetStringFileStatus :: (Applicative m, Monad m) => DirActions m
                          -> TargetString -> m TargetStringFileStatus
getTargetStringFileStatus DirActions{..} t =
    case t of
      TargetString1 s1 ->
        (\f1 -> TargetStringFileStatus1 s1 f1)          <$> fileStatus s1
      TargetString2 s1 s2 ->
        (\f1 -> TargetStringFileStatus2 s1 f1 s2)       <$> fileStatus s1
      TargetString3 s1 s2 s3 ->
        (\f1 -> TargetStringFileStatus3 s1 f1 s2 s3)    <$> fileStatus s1
      TargetString4 s1 s2 s3 s4 ->
        return (TargetStringFileStatus4 s1 s2 s3 s4)
      TargetString5 s1 s2 s3 s4 s5 ->
        return (TargetStringFileStatus5 s1 s2 s3 s4 s5)
      TargetString7 s1 s2 s3 s4 s5 s6 s7 ->
        return (TargetStringFileStatus7 s1 s2 s3 s4 s5 s6 s7)
  where
    fileStatus f = do
      fexists <- doesFileExist f
      dexists <- doesDirectoryExist f
      case splitPath f of
        _ | fexists -> FileStatusExistsFile <$> canonicalizePath f
          | dexists -> FileStatusExistsDir  <$> canonicalizePath f
        (d:_)       -> FileStatusNotExists  <$> doesDirectoryExist d
        _           -> pure (FileStatusNotExists False)

forgetFileStatus :: TargetStringFileStatus -> TargetString
forgetFileStatus t = case t of
    TargetStringFileStatus1 s1 _          -> TargetString1 s1
    TargetStringFileStatus2 s1 _ s2       -> TargetString2 s1 s2
    TargetStringFileStatus3 s1 _ s2 s3    -> TargetString3 s1 s2 s3
    TargetStringFileStatus4 s1   s2 s3 s4 -> TargetString4 s1 s2 s3 s4
    TargetStringFileStatus5 s1   s2 s3 s4
                                       s5 -> TargetString5 s1 s2 s3 s4 s5
    TargetStringFileStatus7 s1   s2 s3 s4
                                 s5 s6 s7 -> TargetString7 s1 s2 s3 s4 s5 s6 s7


-- ------------------------------------------------------------
-- * Resolving target strings to target selectors
-- ------------------------------------------------------------


-- | Given a bunch of user-specified targets, try to resolve what it is they
-- refer to.
--
resolveTargetSelectors :: [PackageInfo]     -- any pkg in the cur dir
                       -> [PackageInfo]     -- all the other local packages
                       -> [TargetStringFileStatus]
                       -> ([TargetSelectorProblem],
                           [TargetSelector PackageInfo])

-- default local dir target if there's no given target:
resolveTargetSelectors [] [] [] =
    ([TargetSelectorNoTargetsInProject], [])

resolveTargetSelectors [] _opinfo [] =
    ([TargetSelectorNoTargetsInCwd], [])

resolveTargetSelectors ppinfo _opinfo [] =
    ([], [TargetPackage TargetImplicitCwd (head ppinfo) Nothing])
    --TODO: in future allow multiple packages in the same dir

resolveTargetSelectors ppinfo opinfo targetStrs =
    partitionEithers
  . map (resolveTargetSelector ppinfo opinfo)
  $ targetStrs

resolveTargetSelector :: [PackageInfo] -> [PackageInfo]
                      -> TargetStringFileStatus
                      -> Either TargetSelectorProblem
                                (TargetSelector PackageInfo)
resolveTargetSelector ppinfo opinfo targetStrStatus =
    case findMatch (matcher targetStrStatus) of

      Unambiguous _
        | projectIsEmpty -> Left TargetSelectorNoTargetsInProject

      Unambiguous (TargetPackage TargetImplicitCwd _ mkfilter)
        | null ppinfo -> Left (TargetSelectorNoCurrentPackage targetStr)
        | otherwise   -> Right (TargetPackage TargetImplicitCwd
                                              (head ppinfo) mkfilter)
                       --TODO: in future allow multiple packages in the same dir

      Unambiguous target -> Right target

      None errs
        | TargetStringFileStatus1 str _ <- targetStrStatus
        , validPackageName str -> Right (TargetPackageName (mkPackageName str))
        | projectIsEmpty       -> Left TargetSelectorNoTargetsInProject
        | otherwise            -> Left (classifyMatchErrors errs)

      Ambiguous exactMatch targets ->
        case disambiguateTargetSelectors
               matcher targetStrStatus exactMatch
               targets of
          Right targets'   -> Left (TargetSelectorAmbiguous targetStr
                                       (map (fmap (fmap packageId)) targets'))
          Left ((m, ms):_) -> Left (MatchingInternalError targetStr
                                       (fmap packageId m)
                                       (map (fmap (map (fmap packageId))) ms))
          Left []          -> internalError "resolveTargetSelector"
  where
    matcher = matchTargetSelector ppinfo opinfo

    targetStr = forgetFileStatus targetStrStatus

    projectIsEmpty = null ppinfo && null opinfo

    classifyMatchErrors errs
      | not (null expected)
      = let (things, got:_) = unzip expected in
        TargetSelectorExpected targetStr things got

      | not (null nosuch)
      = TargetSelectorNoSuch targetStr nosuch

      | otherwise
      = internalError $ "classifyMatchErrors: " ++ show errs
      where
        expected = [ (thing, got)
                   | (_, MatchErrorExpected thing got)
                           <- map (innerErr Nothing) errs ]
        -- Trim the list of alternatives by dropping duplicates and
        -- retaining only at most three most similar (by edit distance) ones.
        nosuch   = Map.foldrWithKey genResults [] $ Map.fromListWith Set.union $
          [ ((inside, thing, got), Set.fromList alts)
          | (inside, MatchErrorNoSuch thing got alts)
            <- map (innerErr Nothing) errs
          ]

        genResults (inside, thing, got) alts acc = (
            inside
          , thing
          , got
          , take maxResults
            $ map fst
            $ takeWhile distanceLow
            $ sortBy (comparing snd)
            $ map addLevDist
            $ Set.toList alts
          ) : acc
          where
            addLevDist = id &&& restrictedDamerauLevenshteinDistance
                                defaultEditCosts got

            distanceLow (_, dist) = dist < length got `div` 2

            maxResults = 3

        innerErr _ (MatchErrorIn kind thing m)
                     = innerErr (Just (kind,thing)) m
        innerErr c m = (c,m)

-- | The various ways that trying to resolve a 'TargetString' to a
-- 'TargetSelector' can fail.
--
data TargetSelectorProblem
   = TargetSelectorExpected TargetString [String]  String
     -- ^  [expected thing] (actually got)
   | TargetSelectorNoSuch  TargetString
                           [(Maybe (String, String), String, String, [String])]
     -- ^ [([in thing], no such thing,  actually got, alternatives)]
   | TargetSelectorAmbiguous  TargetString
                              [(TargetString, TargetSelector PackageId)]

   | MatchingInternalError TargetString (TargetSelector PackageId)
                           [(TargetString, [TargetSelector PackageId])]
   | TargetSelectorUnrecognised String
     -- ^ Syntax error when trying to parse a target string.
   | TargetSelectorNoCurrentPackage TargetString
   | TargetSelectorNoTargetsInCwd
   | TargetSelectorNoTargetsInProject
  deriving (Show, Eq)

data QualLevel = QL1 | QL2 | QL3 | QLFull
  deriving (Eq, Enum, Show)

disambiguateTargetSelectors
  :: (TargetStringFileStatus -> Match (TargetSelector PackageInfo))
  -> TargetStringFileStatus -> Bool
  -> [TargetSelector PackageInfo]
  -> Either [(TargetSelector PackageInfo,
              [(TargetString, [TargetSelector PackageInfo])])]
            [(TargetString, TargetSelector PackageInfo)]
disambiguateTargetSelectors matcher matchInput exactMatch matchResults =
    case partitionEithers results of
      (errs@(_:_), _) -> Left errs
      ([], ok)        -> Right ok
  where
    -- So, here's the strategy. We take the original match results, and make a
    -- table of all their renderings at all qualification levels.
    -- Note there can be multiple renderings at each qualification level.
    matchResultsRenderings :: [(TargetSelector PackageInfo,
                                [TargetStringFileStatus])]
    matchResultsRenderings =
      [ (matchResult, matchRenderings)
      | matchResult <- matchResults
      , let matchRenderings =
              [ rendering
              | ql <- [QL1 .. QLFull]
              , rendering <- renderTargetSelector ql matchResult ]
      ]

    -- Of course the point is that we're looking for renderings that are
    -- unambiguous matches. So we build another memo table of all the matches
    -- for all of those renderings. So by looking up in this table we can see
    -- if we've got an unambiguous match.

    memoisedMatches :: Map TargetStringFileStatus
                           (Match (TargetSelector PackageInfo))
    memoisedMatches =
        -- avoid recomputing the main one if it was an exact match
        (if exactMatch then Map.insert matchInput (ExactMatch 0 matchResults)
                       else id)
      $ Map.Lazy.fromList
          [ (rendering, matcher rendering)
          | rendering <- concatMap snd matchResultsRenderings ]

    -- Finally, for each of the match results, we go through all their
    -- possible renderings (in order of qualification level, though remember
    -- there can be multiple renderings per level), and find the first one
    -- that has an unambiguous match.
    results :: [Either (TargetSelector PackageInfo,
                        [(TargetString, [TargetSelector PackageInfo])])
                       (TargetString, TargetSelector PackageInfo)]
    results =
      [ case findUnambiguous originalMatch matchRenderings of
          Just unambiguousRendering ->
            Right ( forgetFileStatus unambiguousRendering
                  , originalMatch)

          -- This case is an internal error, but we bubble it up and report it
          Nothing ->
            Left  ( originalMatch
                  , [ (forgetFileStatus rendering, matches)
                    | rendering <- matchRenderings
                    , let (ExactMatch _ matches) =
                            memoisedMatches Map.! rendering
                    ] )

      | (originalMatch, matchRenderings) <- matchResultsRenderings ]

    findUnambiguous :: TargetSelector PackageInfo
                    -> [TargetStringFileStatus]
                    -> Maybe TargetStringFileStatus
    findUnambiguous _ []     = Nothing
    findUnambiguous t (r:rs) =
      case memoisedMatches Map.! r of
        ExactMatch _ [t'] | fmap packageName t == fmap packageName t'
                         -> Just r
        ExactMatch _  _  -> findUnambiguous t rs
        InexactMatch _ _ -> internalError "InexactMatch"
        NoMatch      _ _ -> internalError "NoMatch"

internalError :: String -> a
internalError msg =
  error $ "TargetSelector: internal error: " ++ msg


-- | Throw an exception with a formatted message if there are any problems.
--
reportTargetSelectorProblems :: Verbosity -> [TargetSelectorProblem] -> IO a
reportTargetSelectorProblems verbosity problems = do

    case [ str | TargetSelectorUnrecognised str <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [ "Unrecognised target syntax for '" ++ name ++ "'."
          | name <- targets ]

    case [ (t, m, ms) | MatchingInternalError t m ms <- problems ] of
      [] -> return ()
      ((target, originalMatch, renderingsAndMatches):_) ->
        die' verbosity $ "Internal error in target matching. It should always "
           ++ "be possible to find a syntax that's sufficiently qualified to "
           ++ "give an unambiguous match. However when matching '"
           ++ showTargetString target ++ "'  we found "
           ++ showTargetSelector originalMatch
           ++ " (" ++ showTargetSelectorKind originalMatch ++ ") which does "
           ++ "not have an unambiguous syntax. The possible syntax and the "
           ++ "targets they match are as follows:\n"
           ++ unlines
                [ "'" ++ showTargetString rendering ++ "' which matches "
                  ++ intercalate ", "
                       [ showTargetSelector match ++
                         " (" ++ showTargetSelectorKind match ++ ")"
                       | match <- matches ]
                | (rendering, matches) <- renderingsAndMatches ]

    case [ (t, e, g) | TargetSelectorExpected t e g <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [    "Unrecognised target '" ++ showTargetString target
            ++ "'.\n"
            ++ "Expected a " ++ intercalate " or " expected
            ++ ", rather than '" ++ got ++ "'."
          | (target, expected, got) <- targets ]

    case [ (t, e) | TargetSelectorNoSuch t e <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [ "Unknown target '" ++ showTargetString target ++
            "'.\n" ++ unlines
            [    (case inside of
                    Just (kind, "")
                            -> "The " ++ kind ++ " has no "
                    Just (kind, thing)
                            -> "The " ++ kind ++ " " ++ thing ++ " has no "
                    Nothing -> "There is no ")
              ++ intercalate " or " [ mungeThing thing ++ " '" ++ got ++ "'"
                                    | (thing, got, _alts) <- nosuch' ] ++ "."
              ++ if null alternatives then "" else
                 "\nPerhaps you meant " ++ intercalate ";\nor "
                 [ "the " ++ thing ++ " '" ++ intercalate "' or '" alts ++ "'?"
                 | (thing, alts) <- alternatives ]
            | (inside, nosuch') <- groupByContainer nosuch
            , let alternatives =
                    [ (thing, alts)
                    | (thing,_got,alts@(_:_)) <- nosuch' ]
            ]
          | (target, nosuch) <- targets
          , let groupByContainer =
                    map (\g@((inside,_,_,_):_) ->
                            (inside, [   (thing,got,alts)
                                     | (_,thing,got,alts) <- g ]))
                  . groupBy ((==)    `on` (\(x,_,_,_) -> x))
                  . sortBy  (compare `on` (\(x,_,_,_) -> x))
          ]
        where
          mungeThing "file" = "file target"
          mungeThing thing  = thing

    case [ (t, ts) | TargetSelectorAmbiguous t ts <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [    "Ambiguous target '" ++ showTargetString target
            ++ "'. It could be:\n "
            ++ unlines [ "   "++ showTargetString ut ++
                         " (" ++ showTargetSelectorKind bt ++ ")"
                       | (ut, bt) <- amb ]
          | (target, amb) <- targets ]

    case [ t | TargetSelectorNoCurrentPackage t <- problems ] of
      []       -> return ()
      target:_ ->
        die' verbosity $
            "The target '" ++ showTargetString target ++ "' refers to the "
         ++ "components in the package in the current directory, but there "
         ++ "is no package in the current directory (or at least not listed "
         ++ "as part of the project)."
        --TODO: report a different error if there is a .cabal file but it's
        -- not a member of the project

    case [ () | TargetSelectorNoTargetsInCwd <- problems ] of
      []  -> return ()
      _:_ ->
        die' verbosity $
            "No targets given and there is no package in the current "
         ++ "directory. Use the target 'all' for all packages in the "
         ++ "project or specify packages or components by name or location. "
         ++ "See 'cabal build --help' for more details on target options."

    case [ () | TargetSelectorNoTargetsInProject <- problems ] of
      []  -> return ()
      _:_ ->
        die' verbosity $
            "There is no <pkgname>.cabal package file or cabal.project file. "
         ++ "To build packages locally you need at minimum a <pkgname>.cabal "
         ++ "file. You can use 'cabal init' to create one.\n"
         ++ "\n"
         ++ "For non-trivial projects you will also want a cabal.project "
         ++ "file in the root directory of your project. This file lists the "
         ++ "packages in your project and all other build configuration. "
         ++ "See the Cabal user guide for full details."

    fail "reportTargetSelectorProblems: internal error"


----------------------------------
-- Syntax type
--

-- | Syntax for the 'TargetSelector': the matcher and renderer
--
data Syntax = Syntax QualLevel Matcher Renderer
            | AmbiguousAlternatives Syntax Syntax
            | ShadowingAlternatives Syntax Syntax

type Matcher  = TargetStringFileStatus -> Match (TargetSelector PackageInfo)
type Renderer = TargetSelector PackageId -> [TargetStringFileStatus]

foldSyntax :: (a -> a -> a) -> (a -> a -> a)
           -> (QualLevel -> Matcher -> Renderer -> a)
           -> (Syntax -> a)
foldSyntax ambiguous unambiguous syntax = go
  where
    go (Syntax ql match render)    = syntax ql match render
    go (AmbiguousAlternatives a b) = ambiguous   (go a) (go b)
    go (ShadowingAlternatives a b) = unambiguous (go a) (go b)


----------------------------------
-- Top level renderer and matcher
--

renderTargetSelector :: Package p => QualLevel -> TargetSelector p
                     -> [TargetStringFileStatus]
renderTargetSelector ql ts =
    foldSyntax
      (++) (++)
      (\ql' _ render -> guard (ql == ql') >> render (fmap packageId ts))
      syntax
  where
    syntax = syntaxForms [] [] -- don't need pinfo for rendering

matchTargetSelector :: [PackageInfo] -> [PackageInfo]
                    -> TargetStringFileStatus
                    -> Match (TargetSelector PackageInfo)
matchTargetSelector ppinfo opinfo = \utarget ->
    nubMatchesBy ((==) `on` (fmap packageName)) $

    let ql = targetQualLevel utarget in
    foldSyntax
      (<|>) (<//>)
      (\ql' match _ -> guard (ql == ql') >> match utarget)
      syntax
  where
    syntax = syntaxForms ppinfo opinfo

    targetQualLevel TargetStringFileStatus1{} = QL1
    targetQualLevel TargetStringFileStatus2{} = QL2
    targetQualLevel TargetStringFileStatus3{} = QL3
    targetQualLevel TargetStringFileStatus4{} = QLFull
    targetQualLevel TargetStringFileStatus5{} = QLFull
    targetQualLevel TargetStringFileStatus7{} = QLFull


----------------------------------
-- Syntax forms
--

-- | All the forms of syntax for 'TargetSelector'.
--
syntaxForms :: [PackageInfo] -> [PackageInfo] -> Syntax
syntaxForms ppinfo opinfo =
    -- The various forms of syntax here are ambiguous in many cases.
    -- Our policy is by default we expose that ambiguity and report
    -- ambiguous matches. In certain cases we override the ambiguity
    -- by having some forms shadow others.
    --
    -- We make modules shadow files because module name "Q" clashes
    -- with file "Q" with no extension but these refer to the same
    -- thing anyway so it's not a useful ambiguity. Other cases are
    -- not ambiguous like "Q" vs "Q.hs" or "Data.Q" vs "Data/Q".

    ambiguousAlternatives
        -- convenient single-component forms
      [ shadowingAlternatives
          [ ambiguousAlternatives
              [ syntaxForm1All
              , syntaxForm1Filter
              , shadowingAlternatives
                  [ syntaxForm1Component pcinfo
                  , syntaxForm1Package   pinfo
                  ]
              ]
          , syntaxForm1Component ocinfo
          , syntaxForm1Module    cinfo
          , syntaxForm1File      pinfo
          ]

        -- two-component partially qualified forms
        -- fully qualified form for 'all'
      , syntaxForm2MetaAll
      , syntaxForm2AllFilter
      , syntaxForm2NamespacePackage pinfo
      , syntaxForm2PackageComponent pinfo
      , syntaxForm2PackageFilter    pinfo
      , syntaxForm2KindComponent    cinfo
      , shadowingAlternatives
          [ syntaxForm2PackageModule   pinfo
          , syntaxForm2PackageFile     pinfo
          ]
      , shadowingAlternatives
          [ syntaxForm2ComponentModule cinfo
          , syntaxForm2ComponentFile   cinfo
          ]

        -- rarely used partially qualified forms
      , syntaxForm3PackageKindComponent   pinfo
      , shadowingAlternatives
          [ syntaxForm3PackageComponentModule pinfo
          , syntaxForm3PackageComponentFile   pinfo
          ]
      , shadowingAlternatives
          [ syntaxForm3KindComponentModule    cinfo
          , syntaxForm3KindComponentFile      cinfo
          ]
      , syntaxForm3NamespacePackageFilter pinfo

        -- fully-qualified forms for all and cwd with filter
      , syntaxForm3MetaAllFilter
      , syntaxForm3MetaCwdFilter

        -- fully-qualified form for package and package with filter
      , syntaxForm3MetaNamespacePackage       pinfo
      , syntaxForm4MetaNamespacePackageFilter pinfo

        -- fully-qualified forms for component, module and file
      , syntaxForm5MetaNamespacePackageKindComponent                pinfo
      , syntaxForm7MetaNamespacePackageKindComponentNamespaceModule pinfo
      , syntaxForm7MetaNamespacePackageKindComponentNamespaceFile   pinfo
      ]
  where
    ambiguousAlternatives = foldr1 AmbiguousAlternatives
    shadowingAlternatives = foldr1 ShadowingAlternatives
    pinfo  = ppinfo ++ opinfo
    cinfo  = concatMap pinfoComponents pinfo
    pcinfo = concatMap pinfoComponents ppinfo
    ocinfo = concatMap pinfoComponents opinfo


-- | Syntax: "all" to select all packages in the project
--
-- > cabal build all
--
syntaxForm1All :: Syntax
syntaxForm1All =
  syntaxForm1 render $ \str1 _fstatus1 -> do
    guardMetaAll str1
    return (TargetAllPackages Nothing)
  where
    render (TargetAllPackages Nothing) =
      [TargetStringFileStatus1 "all" noFileStatus]
    render _ = []

-- | Syntax: filter
--
-- > cabal build tests
--
syntaxForm1Filter :: Syntax
syntaxForm1Filter =
  syntaxForm1 render $ \str1 _fstatus1 -> do
    kfilter <- matchComponentKindFilter str1
    return (TargetPackage TargetImplicitCwd dummyPackageInfo (Just kfilter))
  where
    render (TargetPackage TargetImplicitCwd _ (Just kfilter)) =
      [TargetStringFileStatus1 (dispF kfilter) noFileStatus]
    render _ = []

-- Only used for TargetPackage TargetImplicitCwd
dummyPackageInfo :: PackageInfo
dummyPackageInfo =
    PackageInfo {
      pinfoId          = PackageIdentifier
                           (mkPackageName "dummyPackageInfo")
                           (mkVersion []),
      pinfoDirectory   = unused,
      pinfoPackageFile = unused,
      pinfoComponents  = unused
    }
  where
    unused = error "dummyPackageInfo"

-- | Syntax: package (name, dir or file)
--
-- > cabal build foo
-- > cabal build ../bar ../bar/bar.cabal
--
syntaxForm1Package :: [PackageInfo] -> Syntax
syntaxForm1Package pinfo =
  syntaxForm1 render $ \str1 fstatus1 -> do
    guardPackage            str1 fstatus1
    p <- matchPackage pinfo str1 fstatus1
    return (TargetPackage TargetExplicitNamed p Nothing)
  where
    render (TargetPackage TargetExplicitNamed p Nothing) =
      [TargetStringFileStatus1 (dispP p) noFileStatus]
    render _ = []

-- | Syntax: component
--
-- > cabal build foo
--
syntaxForm1Component :: [ComponentInfo] -> Syntax
syntaxForm1Component cs =
  syntaxForm1 render $ \str1 _fstatus1 -> do
    guardComponentName str1
    c <- matchComponentName cs str1
    return (TargetComponent (cinfoPackage c) (cinfoName c) WholeComponent)
  where
    render (TargetComponent p c WholeComponent) =
      [TargetStringFileStatus1 (dispC p c) noFileStatus]
    render _ = []

-- | Syntax: module
--
-- > cabal build Data.Foo
--
syntaxForm1Module :: [ComponentInfo] -> Syntax
syntaxForm1Module cs =
  syntaxForm1 render $  \str1 _fstatus1 -> do
    guardModuleName str1
    let ms = [ (m,c) | c <- cs, m <- cinfoModules c ]
    (m,c) <- matchModuleNameAnd ms str1
    return (TargetComponent (cinfoPackage c) (cinfoName c) (ModuleTarget m))
  where
    render (TargetComponent _p _c (ModuleTarget m)) =
      [TargetStringFileStatus1 (dispM m) noFileStatus]
    render _ = []

-- | Syntax: file name
--
-- > cabal build Data/Foo.hs bar/Main.hsc
--
syntaxForm1File :: [PackageInfo] -> Syntax
syntaxForm1File ps =
    -- Note there's a bit of an inconsistency here vs the other syntax forms
    -- for files. For the single-part syntax the target has to point to a file
    -- that exists (due to our use of matchPackageDirectoryPrefix), whereas for
    -- all the other forms we don't require that.
  syntaxForm1 render $ \str1 fstatus1 ->
    expecting "file" str1 $ do
    (pkgfile, p) <- matchPackageDirectoryPrefix ps fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      (filepath, c) <- matchComponentFile (pinfoComponents p) pkgfile
      return (TargetComponent p (cinfoName c) (FileTarget filepath))
  where
    render (TargetComponent _p _c (FileTarget f)) =
      [TargetStringFileStatus1 f noFileStatus]
    render _ = []

---

-- | Syntax:  :all
--
-- > cabal build :all
--
syntaxForm2MetaAll :: Syntax
syntaxForm2MetaAll =
  syntaxForm2 render $ \str1 _fstatus1 str2 -> do
    guardNamespaceMeta str1
    guardMetaAll str2
    return (TargetAllPackages Nothing)
  where
    render (TargetAllPackages Nothing) =
      [TargetStringFileStatus2 "" noFileStatus "all"]
    render _ = []

-- | Syntax:  all : filer
--
-- > cabal build all:tests
--
syntaxForm2AllFilter :: Syntax
syntaxForm2AllFilter =
  syntaxForm2 render $ \str1 _fstatus1 str2 -> do
    guardMetaAll str1
    kfilter <- matchComponentKindFilter str2
    return (TargetAllPackages (Just kfilter))
  where
    render (TargetAllPackages (Just kfilter)) =
      [TargetStringFileStatus2 "all" noFileStatus (dispF kfilter)]
    render _ = []

-- | Syntax:  package : filer
--
-- > cabal build foo:tests
--
syntaxForm2PackageFilter :: [PackageInfo] -> Syntax
syntaxForm2PackageFilter ps =
  syntaxForm2 render $ \str1 fstatus1 str2 -> do
    guardPackage         str1 fstatus1
    p <- matchPackage ps str1 fstatus1
    kfilter <- matchComponentKindFilter str2
    return (TargetPackage TargetExplicitNamed p (Just kfilter))
  where
    render (TargetPackage TargetExplicitNamed p (Just kfilter)) =
      [TargetStringFileStatus2 (dispP p) noFileStatus (dispF kfilter)]
    render _ = []

-- | Syntax: pkg : package name
--
-- > cabal build pkg:foo
--
syntaxForm2NamespacePackage :: [PackageInfo] -> Syntax
syntaxForm2NamespacePackage pinfo =
  syntaxForm2 render $ \str1 _fstatus1 str2 -> do
    guardNamespacePackage   str1
    guardPackageName        str2
    p <- matchPackage pinfo str2 noFileStatus
    return (TargetPackage TargetExplicitNamed p Nothing)
  where
    render (TargetPackage TargetExplicitNamed p Nothing) =
      [TargetStringFileStatus2 "pkg" noFileStatus (dispP p)]
    render _ = []

-- | Syntax: package : component
--
-- > cabal build foo:foo
-- > cabal build ./foo:foo
-- > cabal build ./foo.cabal:foo
--
syntaxForm2PackageComponent :: [PackageInfo] -> Syntax
syntaxForm2PackageComponent ps =
  syntaxForm2 render $ \str1 fstatus1 str2 -> do
    guardPackage         str1 fstatus1
    guardComponentName   str2
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentName (pinfoComponents p) str2
      return (TargetComponent p (cinfoName c) WholeComponent)
    --TODO: the error here ought to say there's no component by that name in
    -- this package, and name the package
  where
    render (TargetComponent p c WholeComponent) =
      [TargetStringFileStatus2 (dispP p) noFileStatus (dispC p c)]
    render _ = []

-- | Syntax: namespace : component
--
-- > cabal build lib:foo exe:foo
--
syntaxForm2KindComponent :: [ComponentInfo] -> Syntax
syntaxForm2KindComponent cs =
  syntaxForm2 render $ \str1 _fstatus1 str2 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    return (TargetComponent (cinfoPackage c) (cinfoName c) WholeComponent)
  where
    render (TargetComponent p c WholeComponent) =
      [TargetStringFileStatus2 (dispK c) noFileStatus (dispC p c)]
    render _ = []

-- | Syntax: package : module
--
-- > cabal build foo:Data.Foo
-- > cabal build ./foo:Data.Foo
-- > cabal build ./foo.cabal:Data.Foo
--
syntaxForm2PackageModule :: [PackageInfo] -> Syntax
syntaxForm2PackageModule ps =
  syntaxForm2 render $ \str1 fstatus1 str2 -> do
    guardPackage         str1 fstatus1
    guardModuleName      str2
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      let ms = [ (m,c) | c <- pinfoComponents p, m <- cinfoModules c ]
      (m,c) <- matchModuleNameAnd ms str2
      return (TargetComponent p (cinfoName c) (ModuleTarget m))
  where
    render (TargetComponent p _c (ModuleTarget m)) =
      [TargetStringFileStatus2 (dispP p) noFileStatus (dispM m)]
    render _ = []

-- | Syntax: component : module
--
-- > cabal build foo:Data.Foo
--
syntaxForm2ComponentModule :: [ComponentInfo] -> Syntax
syntaxForm2ComponentModule cs =
  syntaxForm2 render $ \str1 _fstatus1 str2 -> do
    guardComponentName str1
    guardModuleName    str2
    c <- matchComponentName cs str1
    orNoThingIn "component" (cinfoStrName c) $ do
      let ms = cinfoModules c
      m <- matchModuleName ms str2
      return (TargetComponent (cinfoPackage c) (cinfoName c)
                              (ModuleTarget m))
  where
    render (TargetComponent p c (ModuleTarget m)) =
      [TargetStringFileStatus2 (dispC p c) noFileStatus (dispM m)]
    render _ = []

-- | Syntax: package : filename
--
-- > cabal build foo:Data/Foo.hs
-- > cabal build ./foo:Data/Foo.hs
-- > cabal build ./foo.cabal:Data/Foo.hs
--
syntaxForm2PackageFile :: [PackageInfo] -> Syntax
syntaxForm2PackageFile ps =
  syntaxForm2 render $ \str1 fstatus1 str2 -> do
    guardPackage         str1 fstatus1
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      (filepath, c) <- matchComponentFile (pinfoComponents p) str2
      return (TargetComponent p (cinfoName c) (FileTarget filepath))
  where
    render (TargetComponent p _c (FileTarget f)) =
      [TargetStringFileStatus2 (dispP p) noFileStatus f]
    render _ = []

-- | Syntax: component : filename
--
-- > cabal build foo:Data/Foo.hs
--
syntaxForm2ComponentFile :: [ComponentInfo] -> Syntax
syntaxForm2ComponentFile cs =
  syntaxForm2 render $ \str1 _fstatus1 str2 -> do
    guardComponentName str1
    c <- matchComponentName cs str1
    orNoThingIn "component" (cinfoStrName c) $ do
      (filepath, _) <- matchComponentFile [c] str2
      return (TargetComponent (cinfoPackage c) (cinfoName c)
                              (FileTarget filepath))
  where
    render (TargetComponent p c (FileTarget f)) =
      [TargetStringFileStatus2 (dispC p c) noFileStatus f]
    render _ = []

---

-- | Syntax: :all : filter
--
-- > cabal build :all:tests
--
syntaxForm3MetaAllFilter :: Syntax
syntaxForm3MetaAllFilter =
  syntaxForm3 render $ \str1 _fstatus1 str2 str3 -> do
    guardNamespaceMeta str1
    guardMetaAll str2
    kfilter <- matchComponentKindFilter str3
    return (TargetAllPackages (Just kfilter))
  where
    render (TargetAllPackages (Just kfilter)) =
      [TargetStringFileStatus3 "" noFileStatus "all" (dispF kfilter)]
    render _ = []

syntaxForm3MetaCwdFilter :: Syntax
syntaxForm3MetaCwdFilter =
  syntaxForm3 render $ \str1 _fstatus1 str2 str3 -> do
    guardNamespaceMeta str1
    guardNamespaceCwd str2
    kfilter <- matchComponentKindFilter str3
    return (TargetPackage TargetImplicitCwd dummyPackageInfo (Just kfilter))
  where
    render (TargetPackage TargetImplicitCwd _ (Just kfilter)) =
      [TargetStringFileStatus3 "" noFileStatus "cwd" (dispF kfilter)]
    render _ = []

-- | Syntax: :pkg : package name
--
-- > cabal build :pkg:foo
--
syntaxForm3MetaNamespacePackage :: [PackageInfo] -> Syntax
syntaxForm3MetaNamespacePackage pinfo =
  syntaxForm3 render $ \str1 _fstatus1 str2 str3 -> do
    guardNamespaceMeta      str1
    guardNamespacePackage   str2
    guardPackageName        str3
    p <- matchPackage pinfo str3 noFileStatus
    return (TargetPackage TargetExplicitNamed p Nothing)
  where
    render (TargetPackage TargetExplicitNamed p Nothing) =
      [TargetStringFileStatus3 "" noFileStatus "pkg" (dispP p)]
    render _ = []

-- | Syntax: package : namespace : component
--
-- > cabal build foo:lib:foo
-- > cabal build foo/:lib:foo
-- > cabal build foo.cabal:lib:foo
--
syntaxForm3PackageKindComponent :: [PackageInfo] -> Syntax
syntaxForm3PackageKindComponent ps =
  syntaxForm3 render $ \str1 fstatus1 str2 str3 -> do
    guardPackage         str1 fstatus1
    ckind <- matchComponentKind str2
    guardComponentName   str3
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str3
      return (TargetComponent p (cinfoName c) WholeComponent)
  where
    render (TargetComponent p c WholeComponent) =
      [TargetStringFileStatus3 (dispP p) noFileStatus (dispK c) (dispC p c)]
    render _ = []

-- | Syntax: package : component : module
--
-- > cabal build foo:foo:Data.Foo
-- > cabal build foo/:foo:Data.Foo
-- > cabal build foo.cabal:foo:Data.Foo
--
syntaxForm3PackageComponentModule :: [PackageInfo] -> Syntax
syntaxForm3PackageComponentModule ps =
  syntaxForm3 render $ \str1 fstatus1 str2 str3 -> do
    guardPackage str1 fstatus1
    guardComponentName str2
    guardModuleName    str3
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentName (pinfoComponents p) str2
      orNoThingIn "component" (cinfoStrName c) $ do
        let ms = cinfoModules c
        m <- matchModuleName ms str3
        return (TargetComponent p (cinfoName c) (ModuleTarget m))
  where
    render (TargetComponent p c (ModuleTarget m)) =
      [TargetStringFileStatus3 (dispP p) noFileStatus (dispC p c) (dispM m)]
    render _ = []

-- | Syntax: namespace : component : module
--
-- > cabal build lib:foo:Data.Foo
--
syntaxForm3KindComponentModule :: [ComponentInfo] -> Syntax
syntaxForm3KindComponentModule cs =
  syntaxForm3 render $ \str1 _fstatus1 str2 str3 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    guardModuleName    str3
    c <- matchComponentKindAndName cs ckind str2
    orNoThingIn "component" (cinfoStrName c) $ do
      let ms = cinfoModules c
      m <- matchModuleName ms str3
      return (TargetComponent (cinfoPackage c) (cinfoName c)
                              (ModuleTarget m))
  where
    render (TargetComponent p c (ModuleTarget m)) =
      [TargetStringFileStatus3 (dispK c) noFileStatus (dispC p c) (dispM m)]
    render _ = []

-- | Syntax: package : component : filename
--
-- > cabal build foo:foo:Data/Foo.hs
-- > cabal build foo/:foo:Data/Foo.hs
-- > cabal build foo.cabal:foo:Data/Foo.hs
--
syntaxForm3PackageComponentFile :: [PackageInfo] -> Syntax
syntaxForm3PackageComponentFile ps =
  syntaxForm3 render $ \str1 fstatus1 str2 str3 -> do
    guardPackage         str1 fstatus1
    guardComponentName   str2
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentName (pinfoComponents p) str2
      orNoThingIn "component" (cinfoStrName c) $ do
        (filepath, _) <- matchComponentFile [c] str3
        return (TargetComponent p (cinfoName c) (FileTarget filepath))
  where
    render (TargetComponent p c (FileTarget f)) =
      [TargetStringFileStatus3 (dispP p) noFileStatus (dispC p c) f]
    render _ = []

-- | Syntax: namespace : component : filename
--
-- > cabal build lib:foo:Data/Foo.hs
--
syntaxForm3KindComponentFile :: [ComponentInfo] -> Syntax
syntaxForm3KindComponentFile cs =
  syntaxForm3 render $ \str1 _fstatus1 str2 str3 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    orNoThingIn "component" (cinfoStrName c) $ do
      (filepath, _) <- matchComponentFile [c] str3
      return (TargetComponent (cinfoPackage c) (cinfoName c)
                              (FileTarget filepath))
  where
    render (TargetComponent p c (FileTarget f)) =
      [TargetStringFileStatus3 (dispK c) noFileStatus (dispC p c) f]
    render _ = []

syntaxForm3NamespacePackageFilter :: [PackageInfo] -> Syntax
syntaxForm3NamespacePackageFilter ps =
  syntaxForm3 render $ \str1 _fstatus1 str2 str3 -> do
    guardNamespacePackage str1
    guardPackageName      str2
    p <- matchPackage  ps str2 noFileStatus
    kfilter <- matchComponentKindFilter str3
    return (TargetPackage TargetExplicitNamed p (Just kfilter))
  where
    render (TargetPackage TargetExplicitNamed p (Just kfilter)) =
      [TargetStringFileStatus3 "pkg" noFileStatus (dispP p) (dispF kfilter)]
    render _ = []

--

syntaxForm4MetaNamespacePackageFilter :: [PackageInfo] -> Syntax
syntaxForm4MetaNamespacePackageFilter ps =
  syntaxForm4 render $ \str1 str2 str3 str4 -> do
    guardNamespaceMeta    str1
    guardNamespacePackage str2
    guardPackageName      str3
    p <- matchPackage  ps str3 noFileStatus
    kfilter <- matchComponentKindFilter str4
    return (TargetPackage TargetExplicitNamed p (Just kfilter))
  where
    render (TargetPackage TargetExplicitNamed p (Just kfilter)) =
      [TargetStringFileStatus4 "" "pkg" (dispP p) (dispF kfilter)]
    render _ = []

-- | Syntax: :pkg : package : namespace : component
--
-- > cabal build :pkg:foo:lib:foo
--
syntaxForm5MetaNamespacePackageKindComponent :: [PackageInfo] -> Syntax
syntaxForm5MetaNamespacePackageKindComponent ps =
  syntaxForm5 render $ \str1 str2 str3 str4 str5 -> do
    guardNamespaceMeta    str1
    guardNamespacePackage str2
    guardPackageName      str3
    ckind <- matchComponentKind str4
    guardComponentName    str5
    p <- matchPackage  ps str3 noFileStatus
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str5
      return (TargetComponent p (cinfoName c) WholeComponent)
  where
    render (TargetComponent p c WholeComponent) =
      [TargetStringFileStatus5 "" "pkg" (dispP p) (dispK c) (dispC p c)]
    render _ = []

-- | Syntax: :pkg : package : namespace : component : module : module
--
-- > cabal build :pkg:foo:lib:foo:module:Data.Foo
--
syntaxForm7MetaNamespacePackageKindComponentNamespaceModule
  :: [PackageInfo] -> Syntax
syntaxForm7MetaNamespacePackageKindComponentNamespaceModule ps =
  syntaxForm7 render $ \str1 str2 str3 str4 str5 str6 str7 -> do
    guardNamespaceMeta    str1
    guardNamespacePackage str2
    guardPackageName      str3
    ckind <- matchComponentKind str4
    guardComponentName    str5
    guardNamespaceModule  str6
    p <- matchPackage  ps str3 noFileStatus
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str5
      orNoThingIn "component" (cinfoStrName c) $ do
        let ms = cinfoModules c
        m <- matchModuleName ms str7
        return (TargetComponent p (cinfoName c) (ModuleTarget m))
  where
    render (TargetComponent p c (ModuleTarget m)) =
      [TargetStringFileStatus7 "" "pkg" (dispP p)
                               (dispK c) (dispC p c)
                               "module" (dispM m)]
    render _ = []

-- | Syntax: :pkg : package : namespace : component : file : filename
--
-- > cabal build :pkg:foo:lib:foo:file:Data/Foo.hs
--
syntaxForm7MetaNamespacePackageKindComponentNamespaceFile
  :: [PackageInfo] -> Syntax
syntaxForm7MetaNamespacePackageKindComponentNamespaceFile ps =
  syntaxForm7 render $ \str1 str2 str3 str4 str5 str6 str7 -> do
    guardNamespaceMeta    str1
    guardNamespacePackage str2
    guardPackageName      str3
    ckind <- matchComponentKind str4
    guardComponentName    str5
    guardNamespaceFile    str6
    p <- matchPackage  ps str3 noFileStatus
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str5
      orNoThingIn "component" (cinfoStrName c) $ do
        (filepath,_) <- matchComponentFile [c] str7
        return (TargetComponent p (cinfoName c) (FileTarget filepath))
  where
    render (TargetComponent p c (FileTarget f)) =
      [TargetStringFileStatus7 "" "pkg" (dispP p)
                               (dispK c) (dispC p c)
                               "file" f]
    render _ = []


---------------------------------------
-- Syntax utils
--

type Match1 = String -> FileStatus -> Match (TargetSelector PackageInfo)
type Match2 = String -> FileStatus -> String
              -> Match (TargetSelector PackageInfo)
type Match3 = String -> FileStatus -> String -> String
              -> Match (TargetSelector PackageInfo)
type Match4 = String -> String -> String -> String
              -> Match (TargetSelector PackageInfo)
type Match5 = String -> String -> String -> String -> String
              -> Match (TargetSelector PackageInfo)
type Match7 = String -> String -> String -> String -> String -> String -> String
              -> Match (TargetSelector PackageInfo)

syntaxForm1 :: Renderer -> Match1 -> Syntax
syntaxForm2 :: Renderer -> Match2 -> Syntax
syntaxForm3 :: Renderer -> Match3 -> Syntax
syntaxForm4 :: Renderer -> Match4 -> Syntax
syntaxForm5 :: Renderer -> Match5 -> Syntax
syntaxForm7 :: Renderer -> Match7 -> Syntax

syntaxForm1 render f =
    Syntax QL1 match render
  where
    match = \(TargetStringFileStatus1 str1 fstatus1) ->
              f str1 fstatus1

syntaxForm2 render f =
    Syntax QL2 match render
  where
    match = \(TargetStringFileStatus2 str1 fstatus1 str2) ->
              f str1 fstatus1 str2

syntaxForm3 render f =
    Syntax QL3 match render
  where
    match = \(TargetStringFileStatus3 str1 fstatus1 str2 str3) ->
              f str1 fstatus1 str2 str3

syntaxForm4 render f =
    Syntax QLFull match render
  where
    match (TargetStringFileStatus4 str1 str2 str3 str4)
            = f str1 str2 str3 str4
    match _ = mzero

syntaxForm5 render f =
    Syntax QLFull match render
  where
    match (TargetStringFileStatus5 str1 str2 str3 str4 str5)
            = f str1 str2 str3 str4 str5
    match _ = mzero

syntaxForm7 render f =
    Syntax QLFull match render
  where
    match (TargetStringFileStatus7 str1 str2 str3 str4 str5 str6 str7)
            = f str1 str2 str3 str4 str5 str6 str7
    match _ = mzero

dispP :: Package p => p -> String
dispP = display . packageName

dispC :: Package p => p -> ComponentName -> String
dispC = componentStringName

dispK :: ComponentName -> String
dispK = showComponentKindShort . componentKind

dispF :: ComponentKind -> String
dispF = showComponentKindFilterShort

dispM :: ModuleName -> String
dispM = display


-------------------------------
-- Package and component info
--

data PackageInfo = PackageInfo {
       pinfoId          :: PackageId,
       pinfoDirectory   :: Maybe (FilePath, FilePath),
       pinfoPackageFile :: Maybe (FilePath, FilePath),
       pinfoComponents  :: [ComponentInfo]
     }
  -- not instance of Show due to recursive construction

data ComponentInfo = ComponentInfo {
       cinfoName    :: ComponentName,
       cinfoStrName :: ComponentStringName,
       cinfoPackage :: PackageInfo,
       cinfoSrcDirs :: [FilePath],
       cinfoModules :: [ModuleName],
       cinfoHsFiles :: [FilePath],   -- other hs files (like main.hs)
       cinfoCFiles  :: [FilePath],
       cinfoJsFiles :: [FilePath]
     }
  -- not instance of Show due to recursive construction

type ComponentStringName = String

instance Package PackageInfo where
  packageId = pinfoId

selectPackageInfo :: (Applicative m, Monad m) => DirActions m
                  -> SourcePackage (PackageLocation a) -> m PackageInfo
selectPackageInfo dirActions@DirActions{..}
                  SourcePackage {
                    packageDescription = pkg,
                    packageSource      = loc
                  } = do
    (pkgdir, pkgfile) <-
      case loc of
        --TODO: local tarballs, remote tarballs etc
        LocalUnpackedPackage dir -> do
          dirabs <- canonicalizePath dir
          dirrel <- makeRelativeToCwd dirActions dirabs
          --TODO: ought to get this earlier in project reading
          let fileabs = dirabs </> display (packageName pkg) <.> "cabal"
              filerel = dirrel </> display (packageName pkg) <.> "cabal"
          exists <- doesFileExist fileabs
          return ( Just (dirabs, dirrel)
                 , if exists then Just (fileabs, filerel) else Nothing
                 )
        _ -> return (Nothing, Nothing)
    let pinfo =
          PackageInfo {
            pinfoId          = packageId pkg,
            pinfoDirectory   = pkgdir,
            pinfoPackageFile = pkgfile,
            pinfoComponents  = selectComponentInfo pinfo
                                 (flattenPackageDescription pkg)
          }
    return pinfo


selectComponentInfo :: PackageInfo -> PackageDescription -> [ComponentInfo]
selectComponentInfo pinfo pkg =
    [ ComponentInfo {
        cinfoName    = componentName c,
        cinfoStrName = componentStringName pkg (componentName c),
        cinfoPackage = pinfo,
        cinfoSrcDirs = ordNub (hsSourceDirs bi),
--                       [ pkgroot </> srcdir
--                       | (pkgroot,_) <- maybeToList (pinfoDirectory pinfo)
--                       , srcdir <- hsSourceDirs bi ],
        cinfoModules = ordNub (componentModules c),
        cinfoHsFiles = ordNub (componentHsFiles c),
        cinfoCFiles  = ordNub (cSources bi),
        cinfoJsFiles = ordNub (jsSources bi)
      }
    | c <- pkgComponents pkg
    , let bi = componentBuildInfo c ]


componentStringName :: Package pkg => pkg -> ComponentName -> ComponentStringName
componentStringName pkg CLibName          = display (packageName pkg)
componentStringName _ (CSubLibName name) = unUnqualComponentName name
componentStringName _ (CFLibName name)  = unUnqualComponentName name
componentStringName _ (CExeName   name) = unUnqualComponentName name
componentStringName _ (CTestName  name) = unUnqualComponentName name
componentStringName _ (CBenchName name) = unUnqualComponentName name

componentModules :: Component -> [ModuleName]
-- I think it's unlikely users will ask to build a requirement
-- which is not mentioned locally.
componentModules (CLib   lib)   = explicitLibModules lib
componentModules (CFLib  flib)  = foreignLibModules flib
componentModules (CExe   exe)   = exeModules exe
componentModules (CTest  test)  = testModules test
componentModules (CBench bench) = benchmarkModules bench

componentHsFiles :: Component -> [FilePath]
componentHsFiles (CExe exe) = [modulePath exe]
componentHsFiles (CTest  TestSuite {
                           testInterface = TestSuiteExeV10 _ mainfile
                         }) = [mainfile]
componentHsFiles (CBench Benchmark {
                           benchmarkInterface = BenchmarkExeV10 _ mainfile
                         }) = [mainfile]
componentHsFiles _          = []


------------------------------
-- Matching meta targets
--

guardNamespaceMeta :: String -> Match ()
guardNamespaceMeta = guardToken [""] "meta namespace"

guardMetaAll :: String -> Match ()
guardMetaAll = guardToken ["all"] "meta-target 'all'"

guardNamespacePackage :: String -> Match ()
guardNamespacePackage = guardToken ["pkg", "package"] "'pkg' namespace"

guardNamespaceCwd :: String -> Match ()
guardNamespaceCwd = guardToken ["cwd"] "'cwd' namespace"

guardNamespaceModule :: String -> Match ()
guardNamespaceModule = guardToken ["mod", "module"] "'module' namespace"

guardNamespaceFile :: String -> Match ()
guardNamespaceFile = guardToken ["file"] "'file' namespace"

guardToken :: [String] -> String -> String -> Match ()
guardToken tokens msg s 
  | caseFold s `elem` tokens = increaseConfidence
  | otherwise                = matchErrorExpected msg s


------------------------------
-- Matching component kinds
--

componentKind :: ComponentName -> ComponentKind
componentKind  CLibName      = LibKind
componentKind (CSubLibName _) = LibKind
componentKind (CFLibName _)  = FLibKind
componentKind (CExeName   _) = ExeKind
componentKind (CTestName  _) = TestKind
componentKind (CBenchName _) = BenchKind

cinfoKind :: ComponentInfo -> ComponentKind
cinfoKind = componentKind . cinfoName

matchComponentKind :: String -> Match ComponentKind
matchComponentKind s
  | s' `elem` liblabels   = increaseConfidence >> return LibKind
  | s' `elem` fliblabels  = increaseConfidence >> return FLibKind
  | s' `elem` exelabels   = increaseConfidence >> return ExeKind
  | s' `elem` testlabels  = increaseConfidence >> return TestKind
  | s' `elem` benchlabels = increaseConfidence >> return BenchKind
  | otherwise             = matchErrorExpected "component kind" s
  where
    s'         = caseFold s
    liblabels   = ["lib", "library"]
    fliblabels  = ["flib", "foreign-library"]
    exelabels   = ["exe", "executable"]
    testlabels  = ["tst", "test", "test-suite"]
    benchlabels = ["bench", "benchmark"]

matchComponentKindFilter :: String -> Match ComponentKind
matchComponentKindFilter s
  | s' `elem` liblabels   = increaseConfidence >> return LibKind
  | s' `elem` fliblabels  = increaseConfidence >> return FLibKind
  | s' `elem` exelabels   = increaseConfidence >> return ExeKind
  | s' `elem` testlabels  = increaseConfidence >> return TestKind
  | s' `elem` benchlabels = increaseConfidence >> return BenchKind
  | otherwise             = matchErrorExpected "component kind filter" s
  where
    s'          = caseFold s
    liblabels   = ["libs", "libraries"]
    fliblabels  = ["flibs", "foreign-libraries"]
    exelabels   = ["exes", "executables"]
    testlabels  = ["tests", "test-suites"]
    benchlabels = ["benches", "benchmarks"]

showComponentKind :: ComponentKind -> String
showComponentKind LibKind   = "library"
showComponentKind FLibKind  = "foreign library"
showComponentKind ExeKind   = "executable"
showComponentKind TestKind  = "test-suite"
showComponentKind BenchKind = "benchmark"

showComponentKindShort :: ComponentKind -> String
showComponentKindShort LibKind   = "lib"
showComponentKindShort FLibKind  = "flib"
showComponentKindShort ExeKind   = "exe"
showComponentKindShort TestKind  = "test"
showComponentKindShort BenchKind = "bench"

showComponentKindFilterShort :: ComponentKind -> String
showComponentKindFilterShort LibKind   = "libs"
showComponentKindFilterShort FLibKind  = "flibs"
showComponentKindFilterShort ExeKind   = "exes"
showComponentKindFilterShort TestKind  = "tests"
showComponentKindFilterShort BenchKind = "benchmarks"


------------------------------
-- Matching package targets
--

guardPackage :: String -> FileStatus -> Match ()
guardPackage str fstatus =
      guardPackageName str
  <|> guardPackageDir  str fstatus
  <|> guardPackageFile str fstatus


guardPackageName :: String -> Match ()
guardPackageName s
  | validPackageName s = increaseConfidence
  | otherwise          = matchErrorExpected "package name" s

validPackageName :: String -> Bool
validPackageName s =
       all validPackageNameChar s
    && not (null s)
  where
    validPackageNameChar c = isAlphaNum c || c == '-'


guardPackageDir :: String -> FileStatus -> Match ()
guardPackageDir _ (FileStatusExistsDir _) = increaseConfidence
guardPackageDir str _ = matchErrorExpected "package directory" str


guardPackageFile :: String -> FileStatus -> Match ()
guardPackageFile _ (FileStatusExistsFile file)
                       | takeExtension file == ".cabal"
                       = increaseConfidence
guardPackageFile str _ = matchErrorExpected "package .cabal file" str


matchPackage :: [PackageInfo] -> String -> FileStatus -> Match PackageInfo
matchPackage pinfo = \str fstatus ->
    orNoThingIn "project" "" $
          matchPackageName pinfo str
    <//> (matchPackageDir  pinfo str fstatus
     <|>  matchPackageFile pinfo str fstatus)


matchPackageName :: [PackageInfo] -> String -> Match PackageInfo
matchPackageName ps = \str -> do
    guard (validPackageName str)
    orNoSuchThing "package" str
                  (map (display . packageName) ps) $
      increaseConfidenceFor $
        matchInexactly caseFold (display . packageName) ps str


matchPackageDir :: [PackageInfo]
                -> String -> FileStatus -> Match PackageInfo
matchPackageDir ps = \str fstatus ->
    case fstatus of
      FileStatusExistsDir canondir ->
        orNoSuchThing "package directory" str (map (snd . fst) dirs) $
          increaseConfidenceFor $
            fmap snd $ matchExactly (fst . fst) dirs canondir
      _ -> mzero
  where
    dirs = [ ((dabs,drel),p)
           | p@PackageInfo{ pinfoDirectory = Just (dabs,drel) } <- ps ]


matchPackageFile :: [PackageInfo] -> String -> FileStatus -> Match PackageInfo
matchPackageFile ps = \str fstatus -> do
    case fstatus of
      FileStatusExistsFile canonfile ->
        orNoSuchThing "package .cabal file" str (map (snd . fst) files) $
          increaseConfidenceFor $
            fmap snd $ matchExactly (fst . fst) files canonfile
      _ -> mzero
  where
    files = [ ((fabs,frel),p)
            | p@PackageInfo{ pinfoPackageFile = Just (fabs,frel) } <- ps ]

--TODO: test outcome when dir exists but doesn't match any known one

--TODO: perhaps need another distinction, vs no such thing, point is the
--      thing is not known, within the project, but could be outside project


------------------------------
-- Matching component targets
--


guardComponentName :: String -> Match ()
guardComponentName s
  | all validComponentChar s
    && not (null s)  = increaseConfidence
  | otherwise        = matchErrorExpected "component name" s
  where
    validComponentChar c = isAlphaNum c || c == '.'
                        || c == '_' || c == '-' || c == '\''


matchComponentName :: [ComponentInfo] -> String -> Match ComponentInfo
matchComponentName cs str =
    orNoSuchThing "component" str (map cinfoStrName cs)
  $ increaseConfidenceFor
  $ matchInexactly caseFold cinfoStrName cs str


matchComponentKindAndName :: [ComponentInfo] -> ComponentKind -> String
                          -> Match ComponentInfo
matchComponentKindAndName cs ckind str =
    orNoSuchThing (showComponentKind ckind ++ " component") str
                  (map render cs)
  $ increaseConfidenceFor
  $ matchInexactly (\(ck, cn) -> (ck, caseFold cn))
                   (\c -> (cinfoKind c, cinfoStrName c))
                   cs
                   (ckind, str)
  where
    render c = showComponentKindShort (cinfoKind c) ++ ":" ++ cinfoStrName c


------------------------------
-- Matching module targets
--

guardModuleName :: String -> Match ()
guardModuleName s =
  case simpleParse s :: Maybe ModuleName of
    Just _                   -> increaseConfidence
    _ | all validModuleChar s
        && not (null s)      -> return ()
      | otherwise            -> matchErrorExpected "module name" s
    where
      validModuleChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''


matchModuleName :: [ModuleName] -> String -> Match ModuleName
matchModuleName ms str =
    orNoSuchThing "module" str (map display ms)
  $ increaseConfidenceFor
  $ matchInexactly caseFold display ms str


matchModuleNameAnd :: [(ModuleName, a)] -> String -> Match (ModuleName, a)
matchModuleNameAnd ms str =
    orNoSuchThing "module" str (map (display . fst) ms)
  $ increaseConfidenceFor
  $ matchInexactly caseFold (display . fst) ms str


------------------------------
-- Matching file targets
--

matchPackageDirectoryPrefix :: [PackageInfo] -> FileStatus
                            -> Match (FilePath, PackageInfo)
matchPackageDirectoryPrefix ps (FileStatusExistsFile filepath) =
    increaseConfidenceFor $
      matchDirectoryPrefix pkgdirs filepath
  where
    pkgdirs = [ (dir, p)
              | p@PackageInfo { pinfoDirectory = Just (dir,_) } <- ps ]
matchPackageDirectoryPrefix _ _ = mzero


matchComponentFile :: [ComponentInfo] -> String
                   -> Match (FilePath, ComponentInfo)
matchComponentFile cs str =
    orNoSuchThing "file" str [] $
        matchComponentModuleFile cs str
    <|> matchComponentOtherFile  cs str


matchComponentOtherFile :: [ComponentInfo] -> String
                        -> Match (FilePath, ComponentInfo)
matchComponentOtherFile cs =
    matchFile
      [ (file, c)
      | c    <- cs
      , file <- cinfoHsFiles c
             ++ cinfoCFiles  c
             ++ cinfoJsFiles c
      ]


matchComponentModuleFile :: [ComponentInfo] -> String
                         -> Match (FilePath, ComponentInfo)
matchComponentModuleFile cs str = do
    matchFile
      [ (normalise (d </> toFilePath m), c)
      | c <- cs
      , d <- cinfoSrcDirs c
      , m <- cinfoModules c
      ]
      (dropExtension (normalise str))

-- utils

matchFile :: [(FilePath, a)] -> FilePath -> Match (FilePath, a)
matchFile fs =
      increaseConfidenceFor
    . matchInexactly caseFold fst fs

matchDirectoryPrefix :: [(FilePath, a)] -> FilePath -> Match (FilePath, a)
matchDirectoryPrefix dirs filepath =
    tryEach $
      [ (file, x)
      | (dir,x) <- dirs
      , file <- maybeToList (stripDirectory dir) ]
  where
    stripDirectory :: FilePath -> Maybe FilePath
    stripDirectory dir =
      joinPath `fmap` stripPrefix (splitDirectories dir) filepathsplit

    filepathsplit = splitDirectories filepath


------------------------------
-- Matching monad
--

-- | A matcher embodies a way to match some input as being some recognised
-- value. In particular it deals with multiple and ambiguous matches.
--
-- There are various matcher primitives ('matchExactly', 'matchInexactly'),
-- ways to combine matchers ('matchPlus', 'matchPlusShadowing') and finally we
-- can run a matcher against an input using 'findMatch'.
--
data Match a = NoMatch      Confidence [MatchError]
             | ExactMatch   Confidence [a]
             | InexactMatch Confidence [a]
  deriving Show

type Confidence = Int

data MatchError = MatchErrorExpected String String            -- thing got
                | MatchErrorNoSuch   String String [String]   -- thing got alts
                | MatchErrorIn       String String MatchError -- kind  thing
  deriving (Show, Eq)


instance Functor Match where
    fmap _ (NoMatch      d ms) = NoMatch      d ms
    fmap f (ExactMatch   d xs) = ExactMatch   d (fmap f xs)
    fmap f (InexactMatch d xs) = InexactMatch d (fmap f xs)

instance Applicative Match where
    pure a = ExactMatch 0 [a]
    (<*>)  = ap

instance Alternative Match where
    empty = NoMatch 0 []
    (<|>) = matchPlus

instance Monad Match where
    return                  = pure
    NoMatch      d ms >>= _ = NoMatch d ms
    ExactMatch   d xs >>= f = addDepth d
                            $ msum (map f xs)
    InexactMatch d xs >>= f = addDepth d . forceInexact
                            $ msum (map f xs)

instance MonadPlus Match where
    mzero = empty
    mplus = matchPlus

(<//>) :: Match a -> Match a -> Match a
(<//>) = matchPlusShadowing

infixl 3 <//>

addDepth :: Confidence -> Match a -> Match a
addDepth d' (NoMatch      d msgs) = NoMatch      (d'+d) msgs
addDepth d' (ExactMatch   d xs)   = ExactMatch   (d'+d) xs
addDepth d' (InexactMatch d xs)   = InexactMatch (d'+d) xs

forceInexact :: Match a -> Match a
forceInexact (ExactMatch d ys) = InexactMatch d ys
forceInexact m                 = m

-- | Combine two matchers. Exact matches are used over inexact matches
-- but if we have multiple exact, or inexact then the we collect all the
-- ambiguous matches.
--
-- This operator is associative, has unit 'mzero' and is also commutative.
--
matchPlus :: Match a -> Match a -> Match a
matchPlus   (ExactMatch   d1 xs)   (ExactMatch   d2 xs') =
  ExactMatch (max d1 d2) (xs ++ xs')
matchPlus a@(ExactMatch   _  _ )   (InexactMatch _  _  ) = a
matchPlus a@(ExactMatch   _  _ )   (NoMatch      _  _  ) = a
matchPlus   (InexactMatch _  _ ) b@(ExactMatch   _  _  ) = b
matchPlus   (InexactMatch d1 xs)   (InexactMatch d2 xs') =
  InexactMatch (max d1 d2) (xs ++ xs')
matchPlus a@(InexactMatch _  _ )   (NoMatch      _  _  ) = a
matchPlus   (NoMatch      _  _ ) b@(ExactMatch   _  _  ) = b
matchPlus   (NoMatch      _  _ ) b@(InexactMatch _  _  ) = b
matchPlus a@(NoMatch      d1 ms) b@(NoMatch      d2 ms')
                                             | d1 >  d2  = a
                                             | d1 <  d2  = b
                                             | otherwise = NoMatch d1 (ms ++ ms')

-- | Combine two matchers. This is similar to 'matchPlus' with the
-- difference that an exact match from the left matcher shadows any exact
-- match on the right. Inexact matches are still collected however.
--
-- This operator is associative, has unit 'mzero' and is not commutative.
--
matchPlusShadowing :: Match a -> Match a -> Match a
matchPlusShadowing a@(ExactMatch _ _)  _ = a
matchPlusShadowing a                   b = matchPlus a b


------------------------------
-- Various match primitives
--

matchErrorExpected :: String -> String -> Match a
matchErrorExpected thing got      = NoMatch 0 [MatchErrorExpected thing got]

matchErrorNoSuch :: String -> String -> [String] -> Match a
matchErrorNoSuch thing got alts = NoMatch 0 [MatchErrorNoSuch thing got alts]

expecting :: String -> String -> Match a -> Match a
expecting thing got (NoMatch 0 _) = matchErrorExpected thing got
expecting _     _   m             = m

orNoSuchThing :: String -> String -> [String] -> Match a -> Match a
orNoSuchThing thing got alts (NoMatch 0 _) = matchErrorNoSuch thing got alts
orNoSuchThing _     _   _    m             = m

orNoThingIn :: String -> String -> Match a -> Match a
orNoThingIn kind name (NoMatch n ms) =
    NoMatch n [ MatchErrorIn kind name m | m <- ms ]
orNoThingIn _ _ m = m

increaseConfidence :: Match ()
increaseConfidence = ExactMatch 1 [()]

increaseConfidenceFor :: Match a -> Match a
increaseConfidenceFor m = m >>= \r -> increaseConfidence >> return r

nubMatchesBy :: (a -> a -> Bool) -> Match a -> Match a
nubMatchesBy _  (NoMatch      d msgs) = NoMatch      d msgs
nubMatchesBy eq (ExactMatch   d xs)   = ExactMatch   d (nubBy eq xs)
nubMatchesBy eq (InexactMatch d xs)   = InexactMatch d (nubBy eq xs)

-- | Lift a list of matches to an exact match.
--
exactMatches, inexactMatches :: [a] -> Match a

exactMatches [] = mzero
exactMatches xs = ExactMatch 0 xs

inexactMatches [] = mzero
inexactMatches xs = InexactMatch 0 xs

tryEach :: [a] -> Match a
tryEach = exactMatches


------------------------------
-- Top level match runner
--

-- | Given a matcher and a key to look up, use the matcher to find all the
-- possible matches. There may be 'None', a single 'Unambiguous' match or
-- you may have an 'Ambiguous' match with several possibilities.
--
findMatch :: Match a -> MaybeAmbiguous a
findMatch match = case match of
  NoMatch    _ msgs  -> None msgs
  ExactMatch   _ [x] -> Unambiguous x
  InexactMatch _ [x] -> Unambiguous x
  ExactMatch   _  [] -> error "findMatch: impossible: ExactMatch []"
  InexactMatch _  [] -> error "findMatch: impossible: InexactMatch []"
  ExactMatch   _  xs -> Ambiguous True  xs
  InexactMatch _  xs -> Ambiguous False xs

data MaybeAmbiguous a = None [MatchError] | Unambiguous a | Ambiguous Bool [a]
  deriving Show


------------------------------
-- Basic matchers
--

-- | A primitive matcher that looks up a value in a finite 'Map'. The
-- value must match exactly.
--
matchExactly :: Ord k => (a -> k) -> [a] -> (k -> Match a)
matchExactly key xs =
    \k -> case Map.lookup k m of
            Nothing -> mzero
            Just ys -> exactMatches ys
  where
    m = Map.fromListWith (++) [ (key x, [x]) | x <- xs ]

-- | A primitive matcher that looks up a value in a finite 'Map'. It checks
-- for an exact or inexact match. We get an inexact match if the match
-- is not exact, but the canonical forms match. It takes a canonicalisation
-- function for this purpose.
--
-- So for example if we used string case fold as the canonicalisation
-- function, then we would get case insensitive matching (but it will still
-- report an exact match when the case matches too).
--
matchInexactly :: (Ord k, Ord k') => (k -> k') -> (a -> k)
               -> [a] -> (k -> Match a)
matchInexactly cannonicalise key xs =
    \k -> case Map.lookup k m of
            Just ys -> exactMatches ys
            Nothing -> case Map.lookup (cannonicalise k) m' of
                         Just ys -> inexactMatches ys
                         Nothing -> mzero
  where
    m = Map.fromListWith (++) [ (key x, [x]) | x <- xs ]

    -- the map of canonicalised keys to groups of inexact matches
    m' = Map.mapKeysWith (++) cannonicalise m


------------------------------
-- Utils
--

caseFold :: String -> String
caseFold = lowercase


------------------------------
-- Example inputs
--

{-
ex1pinfo :: [PackageInfo]
ex1pinfo =
  [ addComponent (CExeName (mkUnqualComponentName "foo-exe")) [] ["Data.Foo"] $
    PackageInfo {
      pinfoId          = PackageIdentifier (mkPackageName "foo") (mkVersion [1]),
      pinfoDirectory   = Just ("/the/foo", "foo"),
      pinfoPackageFile = Just ("/the/foo/foo.cabal", "foo/foo.cabal"),
      pinfoComponents  = []
    }
  , PackageInfo {
      pinfoId          = PackageIdentifier (mkPackageName "bar") (mkVersion [1]),
      pinfoDirectory   = Just ("/the/bar", "bar"),
      pinfoPackageFile = Just ("/the/bar/bar.cabal", "bar/bar.cabal"),
      pinfoComponents  = []
    }
  ]
  where
    addComponent n ds ms p =
      p {
        pinfoComponents =
            ComponentInfo n (componentStringName (pinfoId p) n)
                          p ds (map mkMn ms)
                          [] [] []
          : pinfoComponents p
      }

    mkMn :: String -> ModuleName
    mkMn  = ModuleName.fromString
-}
{-
stargets =
  [ TargetComponent (CExeName "foo")  WholeComponent
  , TargetComponent (CExeName "foo") (ModuleTarget (mkMn "Foo"))
  , TargetComponent (CExeName "tst") (ModuleTarget (mkMn "Foo"))
  ]
    where
    mkMn :: String -> ModuleName
    mkMn  = fromJust . simpleParse

ex_pkgid :: PackageIdentifier
Just ex_pkgid = simpleParse "thelib"
-}

{-
ex_cs :: [ComponentInfo]
ex_cs =
  [ (mkC (CExeName "foo") ["src1", "src1/src2"] ["Foo", "Src2.Bar", "Bar"])
  , (mkC (CExeName "tst") ["src1", "test"]      ["Foo"])
  ]
    where
    mkC n ds ms = ComponentInfo n (componentStringName n) ds (map mkMn ms)
    mkMn :: String -> ModuleName
    mkMn  = fromJust . simpleParse
    pkgid :: PackageIdentifier
    Just pkgid = simpleParse "thelib"
-}
