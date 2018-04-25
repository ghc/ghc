{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Solver.Types.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of packages.
--
module Distribution.Solver.Types.PackageIndex (
  -- * Package index data type
  PackageIndex,

  -- * Creating an index
  fromList,

  -- * Updates
  merge,
  insert,
  deletePackageName,
  deletePackageId,
  deleteDependency,

  -- * Queries

  -- ** Precise lookups
  elemByPackageId,
  elemByPackageName,
  lookupPackageName,
  lookupPackageId,
  lookupDependency,

  -- ** Case-insensitive searches
  searchByName,
  SearchResult(..),
  searchByNameSubstring,

  -- ** Bulk queries
  allPackages,
  allPackagesByName,
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (lookup)

import Control.Exception (assert)
import qualified Data.Map as Map
import Data.List (groupBy, isInfixOf)

import Distribution.Package
         ( PackageName, unPackageName, PackageIdentifier(..)
         , Package(..), packageName, packageVersion )
import Distribution.Types.Dependency
import Distribution.Version
         ( withinRange )
import Distribution.Simple.Utils
         ( lowercase, comparing )


-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched efficiently by package name and version.
--
newtype PackageIndex pkg = PackageIndex
  -- This index package names to all the package records matching that package
  -- name case-sensitively. It includes all versions.
  --
  -- This allows us to find all versions satisfying a dependency.
  -- Most queries are a map lookup followed by a linear scan of the bucket.
  --
  (Map PackageName [pkg])

  deriving (Eq, Show, Read, Functor, Generic)
--FIXME: the Functor instance here relies on no package id changes

instance Package pkg => Semigroup (PackageIndex pkg) where
  (<>) = merge

instance Package pkg => Monoid (PackageIndex pkg) where
  mempty  = PackageIndex Map.empty
  mappend = (<>)
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

instance Binary pkg => Binary (PackageIndex pkg)

invariant :: Package pkg => PackageIndex pkg -> Bool
invariant (PackageIndex m) = all (uncurry goodBucket) (Map.toList m)
  where
    goodBucket _    [] = False
    goodBucket name (pkg0:pkgs0) = check (packageId pkg0) pkgs0
      where
        check pkgid []          = packageName pkgid == name
        check pkgid (pkg':pkgs) = packageName pkgid == name
                               && pkgid < pkgid'
                               && check pkgid' pkgs
          where pkgid' = packageId pkg'

--
-- * Internal helpers
--

mkPackageIndex :: Package pkg => Map PackageName [pkg] -> PackageIndex pkg
mkPackageIndex index = assert (invariant (PackageIndex index))
                                         (PackageIndex index)

internalError :: String -> a
internalError name = error ("PackageIndex." ++ name ++ ": internal error")

-- | Lookup a name in the index to get all packages that match that name
-- case-sensitively.
--
lookup :: PackageIndex pkg -> PackageName -> [pkg]
lookup (PackageIndex m) name = fromMaybe [] $ Map.lookup name m

--
-- * Construction
--

-- | Build an index out of a bunch of packages.
--
-- If there are duplicates, later ones mask earlier ones.
--
fromList :: Package pkg => [pkg] -> PackageIndex pkg
fromList pkgs = mkPackageIndex
              . Map.map fixBucket
              . Map.fromListWith (++)
              $ [ (packageName pkg, [pkg])
                | pkg <- pkgs ]
  where
    fixBucket = -- out of groups of duplicates, later ones mask earlier ones
                -- but Map.fromListWith (++) constructs groups in reverse order
                map head
                -- Eq instance for PackageIdentifier is wrong, so use Ord:
              . groupBy (\a b -> EQ == comparing packageId a b)
                -- relies on sortBy being a stable sort so we
                -- can pick consistently among duplicates
              . sortBy (comparing packageId)

--
-- * Updates
--

-- | Merge two indexes.
--
-- Packages from the second mask packages of the same exact name
-- (case-sensitively) from the first.
--
merge :: Package pkg => PackageIndex pkg -> PackageIndex pkg -> PackageIndex pkg
merge i1@(PackageIndex m1) i2@(PackageIndex m2) =
  assert (invariant i1 && invariant i2) $
    mkPackageIndex (Map.unionWith mergeBuckets m1 m2)

-- | Elements in the second list mask those in the first.
mergeBuckets :: Package pkg => [pkg] -> [pkg] -> [pkg]
mergeBuckets []     ys     = ys
mergeBuckets xs     []     = xs
mergeBuckets xs@(x:xs') ys@(y:ys') =
      case packageId x `compare` packageId y of
        GT -> y : mergeBuckets xs  ys'
        EQ -> y : mergeBuckets xs' ys'
        LT -> x : mergeBuckets xs' ys

-- | Inserts a single package into the index.
--
-- This is equivalent to (but slightly quicker than) using 'mappend' or
-- 'merge' with a singleton index.
--
insert :: Package pkg => pkg -> PackageIndex pkg -> PackageIndex pkg
insert pkg (PackageIndex index) = mkPackageIndex $
  Map.insertWith (\_ -> insertNoDup) (packageName pkg) [pkg] index
  where
    pkgid = packageId pkg
    insertNoDup []                = [pkg]
    insertNoDup pkgs@(pkg':pkgs') = case compare pkgid (packageId pkg') of
      LT -> pkg  : pkgs
      EQ -> pkg  : pkgs'
      GT -> pkg' : insertNoDup pkgs'

-- | Internal delete helper.
--
delete :: Package pkg => PackageName -> (pkg -> Bool) -> PackageIndex pkg
       -> PackageIndex pkg
delete name p (PackageIndex index) = mkPackageIndex $
  Map.update filterBucket name index
  where
    filterBucket = deleteEmptyBucket
                 . filter (not . p)
    deleteEmptyBucket []        = Nothing
    deleteEmptyBucket remaining = Just remaining

-- | Removes a single package from the index.
--
deletePackageId :: Package pkg => PackageIdentifier -> PackageIndex pkg
                -> PackageIndex pkg
deletePackageId pkgid =
  delete (packageName pkgid) (\pkg -> packageId pkg == pkgid)

-- | Removes all packages with this (case-sensitive) name from the index.
--
deletePackageName :: Package pkg => PackageName -> PackageIndex pkg
                  -> PackageIndex pkg
deletePackageName name =
  delete name (\pkg -> packageName pkg == name)

-- | Removes all packages satisfying this dependency from the index.
--
deleteDependency :: Package pkg => Dependency -> PackageIndex pkg
                 -> PackageIndex pkg
deleteDependency (Dependency name verstionRange) =
  delete name (\pkg -> packageVersion pkg `withinRange` verstionRange)

--
-- * Bulk queries
--

-- | Get all the packages from the index.
--
allPackages :: PackageIndex pkg -> [pkg]
allPackages (PackageIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: PackageIndex pkg -> [[pkg]]
allPackagesByName (PackageIndex m) = Map.elems m

--
-- * Lookups
--

elemByPackageId :: Package pkg => PackageIndex pkg -> PackageIdentifier -> Bool
elemByPackageId index = isJust . lookupPackageId index

elemByPackageName :: Package pkg => PackageIndex pkg -> PackageName -> Bool
elemByPackageName index = not . null . lookupPackageName index


-- | Does a lookup by package id (name & version).
--
-- Since multiple package DBs mask each other case-sensitively by package name,
-- then we get back at most one package.
--
lookupPackageId :: Package pkg => PackageIndex pkg -> PackageIdentifier
                -> Maybe pkg
lookupPackageId index pkgid =
  case [ pkg | pkg <- lookup index (packageName pkgid)
             , packageId pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name.
--
lookupPackageName :: Package pkg => PackageIndex pkg -> PackageName -> [pkg]
lookupPackageName index name =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name ]

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: Package pkg => PackageIndex pkg -> Dependency -> [pkg]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name
        , packageVersion pkg `withinRange` versionRange ]

--
-- * Case insensitive name lookups
--

-- | Does a case-insensitive search by package name.
--
-- If there is only one package that compares case-insensitively to this name
-- then the search is unambiguous and we get back all versions of that package.
-- If several match case-insensitively but one matches exactly then it is also
-- unambiguous.
--
-- If however several match case-insensitively and none match exactly then we
-- have an ambiguous result, and we get back all the versions of all the
-- packages. The list of ambiguous results is split by exact package name. So
-- it is a non-empty list of non-empty lists.
--
searchByName :: PackageIndex pkg
             -> String -> [(PackageName, [pkg])]
searchByName (PackageIndex m) name =
    [ pkgs
    | pkgs@(pname,_) <- Map.toList m
    , lowercase (unPackageName pname) == lname ]
  where
    lname = lowercase name

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: PackageIndex pkg
                      -> String -> [(PackageName, [pkg])]
searchByNameSubstring (PackageIndex m) searchterm =
    [ pkgs
    | pkgs@(pname, _) <- Map.toList m
    , lsearchterm `isInfixOf` lowercase (unPackageName pname) ]
  where
    lsearchterm = lowercase searchterm
