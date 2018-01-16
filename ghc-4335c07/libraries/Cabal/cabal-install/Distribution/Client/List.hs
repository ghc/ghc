-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.List
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2008-2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
--
-- Search for and print information about packages
-----------------------------------------------------------------------------
module Distribution.Client.List (
  list, info
  ) where

import Distribution.Package
         ( PackageName, Package(..), packageName
         , packageVersion, UnitId )
import Distribution.Types.Dependency
import Distribution.Types.UnqualComponentName
import Distribution.ModuleName (ModuleName)
import Distribution.License (License)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.PackageDescription   as Source
import Distribution.PackageDescription
         ( Flag(..), unFlagName )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )

import Distribution.Simple.Compiler
        ( Compiler, PackageDBStack )
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Utils
        ( equating, comparing, die', notice )
import Distribution.Simple.Setup (fromFlag)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Version
         ( Version, mkVersion, versionNumbers, VersionRange, withinRange, anyVersion
         , intersectVersionRanges, simplifyVersionRange )
import Distribution.Verbosity (Verbosity)
import Distribution.Text
         ( Text(disp), display )

import           Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import           Distribution.Solver.Types.SourcePackage

import Distribution.Client.Types
         ( SourcePackageDb(..), PackageSpecifier(..), UnresolvedSourcePackage )
import Distribution.Client.Targets
         ( UserTarget, resolveUserTargets )
import Distribution.Client.Setup
         ( GlobalFlags(..), ListFlags(..), InfoFlags(..)
         , RepoContext(..) )
import Distribution.Client.Utils
         ( mergeBy, MergeResult(..) )
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.FetchUtils
         ( isFetched )

import Data.List
         ( sortBy, groupBy, sort, nub, intersperse, maximumBy, partition )
import Data.Maybe
         ( listToMaybe, fromJust, fromMaybe, isJust, maybeToList )
import qualified Data.Map as Map
import Data.Tree as Tree
import Control.Monad
         ( MonadPlus(mplus), join )
import Control.Exception
         ( assert )
import Text.PrettyPrint as Disp
import System.Directory
         ( doesDirectoryExist )


-- | Return a list of packages matching given search strings.
getPkgList :: Verbosity
           -> PackageDBStack
           -> RepoContext
           -> Compiler
           -> ProgramDb
           -> ListFlags
           -> [String]
           -> IO [PackageDisplayInfo]
getPkgList verbosity packageDBs repoCtxt comp progdb listFlags pats = do
    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb       <- getSourcePackages verbosity repoCtxt
    let sourcePkgIndex = packageIndex sourcePkgDb
        prefs name = fromMaybe anyVersion
                       (Map.lookup name (packagePreferences sourcePkgDb))

        pkgsInfo ::
          [(PackageName, [Installed.InstalledPackageInfo], [UnresolvedSourcePackage])]
        pkgsInfo
            -- gather info for all packages
          | null pats = mergePackages
                        (InstalledPackageIndex.allPackages installedPkgIndex)
                        (         PackageIndex.allPackages sourcePkgIndex)

            -- gather info for packages matching search term
          | otherwise = pkgsInfoMatching

        pkgsInfoMatching ::
          [(PackageName, [Installed.InstalledPackageInfo], [UnresolvedSourcePackage])]
        pkgsInfoMatching =
          let matchingInstalled = matchingPackages
                                  InstalledPackageIndex.searchByNameSubstring
                                  installedPkgIndex
              matchingSource   = matchingPackages
                                 (\ idx n ->
                                   concatMap snd
                                   (PackageIndex.searchByNameSubstring idx n))
                                 sourcePkgIndex
          in mergePackages matchingInstalled matchingSource

        matches :: [PackageDisplayInfo]
        matches = [ mergePackageInfo pref
                      installedPkgs sourcePkgs selectedPkg False
                  | (pkgname, installedPkgs, sourcePkgs) <- pkgsInfo
                  , not onlyInstalled || not (null installedPkgs)
                  , let pref        = prefs pkgname
                        selectedPkg = latestWithPref pref sourcePkgs ]
    return matches
  where
    onlyInstalled = fromFlag (listInstalled listFlags)
    matchingPackages search index =
      [ pkg
      | pat <- pats
      , pkg <- search index pat ]


-- | Show information about packages.
list :: Verbosity
     -> PackageDBStack
     -> RepoContext
     -> Compiler
     -> ProgramDb
     -> ListFlags
     -> [String]
     -> IO ()
list verbosity packageDBs repos comp progdb listFlags pats = do
    matches <- getPkgList verbosity packageDBs repos comp progdb listFlags pats

    if simpleOutput
      then putStr $ unlines
             [ display (pkgName pkg) ++ " " ++ display version
             | pkg <- matches
             , version <- if onlyInstalled
                            then              installedVersions pkg
                            else nub . sort $ installedVersions pkg
                                           ++ sourceVersions    pkg ]
             -- Note: this only works because for 'list', one cannot currently
             -- specify any version constraints, so listing all installed
             -- and source ones works.
      else
        if null matches
            then notice verbosity "No matches found."
            else putStr $ unlines (map showPackageSummaryInfo matches)
  where
    onlyInstalled = fromFlag (listInstalled listFlags)
    simpleOutput  = fromFlag (listSimpleOutput listFlags)

info :: Verbosity
     -> PackageDBStack
     -> RepoContext
     -> Compiler
     -> ProgramDb
     -> GlobalFlags
     -> InfoFlags
     -> [UserTarget]
     -> IO ()
info verbosity _ _ _ _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

info verbosity packageDBs repoCtxt comp progdb
     globalFlags _listFlags userTargets = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb       <- getSourcePackages verbosity repoCtxt
    let sourcePkgIndex = packageIndex sourcePkgDb
        prefs name = fromMaybe anyVersion
                       (Map.lookup name (packagePreferences sourcePkgDb))

        -- Users may specify names of packages that are only installed, not
        -- just available source packages, so we must resolve targets using
        -- the combination of installed and source packages.
    let sourcePkgs' = PackageIndex.fromList
                    $ map packageId
                      (InstalledPackageIndex.allPackages installedPkgIndex)
                   ++ map packageId
                      (PackageIndex.allPackages sourcePkgIndex)
    pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                       (fromFlag $ globalWorldFile globalFlags)
                       sourcePkgs' userTargets

    pkgsinfo      <- sequence
                       [ do pkginfo <- either (die' verbosity) return $
                                         gatherPkgInfo prefs
                                           installedPkgIndex sourcePkgIndex
                                           pkgSpecifier
                            updateFileSystemPackageDetails pkginfo
                       | pkgSpecifier <- pkgSpecifiers ]

    putStr $ unlines (map showPackageDetailedInfo pkgsinfo)

  where
    gatherPkgInfo :: (PackageName -> VersionRange) ->
                     InstalledPackageIndex ->
                     PackageIndex.PackageIndex UnresolvedSourcePackage ->
                     PackageSpecifier UnresolvedSourcePackage ->
                     Either String PackageDisplayInfo
    gatherPkgInfo prefs installedPkgIndex sourcePkgIndex
      (NamedPackage name props)
      | null (selectedInstalledPkgs) && null (selectedSourcePkgs)
      = Left $ "There is no available version of " ++ display name
            ++ " that satisfies "
            ++ display (simplifyVersionRange verConstraint)

      | otherwise
      = Right $ mergePackageInfo pref installedPkgs
                                 sourcePkgs  selectedSourcePkg'
                                 showPkgVersion
      where
        (pref, installedPkgs, sourcePkgs) =
          sourcePkgsInfo prefs name installedPkgIndex sourcePkgIndex

        selectedInstalledPkgs = InstalledPackageIndex.lookupDependency
                                installedPkgIndex
                                (Dependency name verConstraint)
        selectedSourcePkgs    = PackageIndex.lookupDependency sourcePkgIndex
                                (Dependency name verConstraint)
        selectedSourcePkg'    = latestWithPref pref selectedSourcePkgs

                         -- display a specific package version if the user
                         -- supplied a non-trivial version constraint
        showPkgVersion = not (null verConstraints)
        verConstraint  = foldr intersectVersionRanges anyVersion verConstraints
        verConstraints = [ vr | PackagePropertyVersion vr <- props ]

    gatherPkgInfo prefs installedPkgIndex sourcePkgIndex
      (SpecificSourcePackage pkg) =
        Right $ mergePackageInfo pref installedPkgs sourcePkgs
                                 selectedPkg True
      where
        name          = packageName pkg
        selectedPkg   = Just pkg
        (pref, installedPkgs, sourcePkgs) =
          sourcePkgsInfo prefs name installedPkgIndex sourcePkgIndex

sourcePkgsInfo ::
  (PackageName -> VersionRange)
  -> PackageName
  -> InstalledPackageIndex
  -> PackageIndex.PackageIndex UnresolvedSourcePackage
  -> (VersionRange, [Installed.InstalledPackageInfo], [UnresolvedSourcePackage])
sourcePkgsInfo prefs name installedPkgIndex sourcePkgIndex =
  (pref, installedPkgs, sourcePkgs)
  where
    pref          = prefs name
    installedPkgs = concatMap snd (InstalledPackageIndex.lookupPackageName
                                   installedPkgIndex name)
    sourcePkgs    = PackageIndex.lookupPackageName sourcePkgIndex name


-- | The info that we can display for each package. It is information per
-- package name and covers all installed and available versions.
--
data PackageDisplayInfo = PackageDisplayInfo {
    pkgName           :: PackageName,
    selectedVersion   :: Maybe Version,
    selectedSourcePkg :: Maybe UnresolvedSourcePackage,
    installedVersions :: [Version],
    sourceVersions    :: [Version],
    preferredVersions :: VersionRange,
    homepage          :: String,
    bugReports        :: String,
    sourceRepo        :: String,
    synopsis          :: String,
    description       :: String,
    category          :: String,
    license           :: License,
    author            :: String,
    maintainer        :: String,
    dependencies      :: [ExtDependency],
    flags             :: [Flag],
    hasLib            :: Bool,
    hasExe            :: Bool,
    executables       :: [UnqualComponentName],
    modules           :: [ModuleName],
    haddockHtml       :: FilePath,
    haveTarball       :: Bool
  }

-- | Covers source dependencies and installed dependencies in
-- one type.
data ExtDependency = SourceDependency Dependency
                   | InstalledDependency UnitId

showPackageSummaryInfo :: PackageDisplayInfo -> String
showPackageSummaryInfo pkginfo =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
     char '*' <+> disp (pkgName pkginfo)
     $+$
     (nest 4 $ vcat [
       maybeShow (synopsis pkginfo) "Synopsis:" reflowParagraphs
     , text "Default available version:" <+>
       case selectedSourcePkg pkginfo of
         Nothing  -> text "[ Not available from any configured repository ]"
         Just pkg -> disp (packageVersion pkg)
     , text "Installed versions:" <+>
       case installedVersions pkginfo of
         []  | hasLib pkginfo -> text "[ Not installed ]"
             | otherwise      -> text "[ Unknown ]"
         versions             -> dispTopVersions 4
                                   (preferredVersions pkginfo) versions
     , maybeShow (homepage pkginfo) "Homepage:" text
     , text "License: " <+> text (display (license pkginfo))
     ])
     $+$ text ""
  where
    maybeShow [] _ _ = empty
    maybeShow l  s f = text s <+> (f l)

showPackageDetailedInfo :: PackageDisplayInfo -> String
showPackageDetailedInfo pkginfo =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
   char '*' <+> disp (pkgName pkginfo)
            <>  maybe empty (\v -> char '-' <> disp v) (selectedVersion pkginfo)
            <+> text (replicate (16 - length (display (pkgName pkginfo))) ' ')
            <>  parens pkgkind
   $+$
   (nest 4 $ vcat [
     entry "Synopsis"      synopsis     hideIfNull     reflowParagraphs
   , entry "Versions available" sourceVersions
           (altText null "[ Not available from server ]")
           (dispTopVersions 9 (preferredVersions pkginfo))
   , entry "Versions installed" installedVersions
           (altText null (if hasLib pkginfo then "[ Not installed ]"
                                            else "[ Unknown ]"))
           (dispTopVersions 4 (preferredVersions pkginfo))
   , entry "Homepage"      homepage     orNotSpecified text
   , entry "Bug reports"   bugReports   orNotSpecified text
   , entry "Description"   description  hideIfNull     reflowParagraphs
   , entry "Category"      category     hideIfNull     text
   , entry "License"       license      alwaysShow     disp
   , entry "Author"        author       hideIfNull     reflowLines
   , entry "Maintainer"    maintainer   hideIfNull     reflowLines
   , entry "Source repo"   sourceRepo   orNotSpecified text
   , entry "Executables"   executables  hideIfNull     (commaSep disp)
   , entry "Flags"         flags        hideIfNull     (commaSep dispFlag)
   , entry "Dependencies"  dependencies hideIfNull     (commaSep dispExtDep)
   , entry "Documentation" haddockHtml  showIfInstalled text
   , entry "Cached"        haveTarball  alwaysShow     dispYesNo
   , if not (hasLib pkginfo) then empty else
     text "Modules:" $+$ nest 4 (vcat (map disp . sort . modules $ pkginfo))
   ])
   $+$ text ""
  where
    entry fname field cond format = case cond (field pkginfo) of
      Nothing           -> label <+> format (field pkginfo)
      Just Nothing      -> empty
      Just (Just other) -> label <+> text other
      where
        label   = text fname <> char ':' <> padding
        padding = text (replicate (13 - length fname ) ' ')

    normal      = Nothing
    hide        = Just Nothing
    replace msg = Just (Just msg)

    alwaysShow = const normal
    hideIfNull v = if null v then hide else normal
    showIfInstalled v
      | not isInstalled = hide
      | null v          = replace "[ Not installed ]"
      | otherwise       = normal
    altText nul msg v = if nul v then replace msg else normal
    orNotSpecified = altText null "[ Not specified ]"

    commaSep f = Disp.fsep . Disp.punctuate (Disp.char ',') . map f
    dispFlag = text . unFlagName . flagName
    dispYesNo True  = text "Yes"
    dispYesNo False = text "No"

    dispExtDep (SourceDependency    dep) = disp dep
    dispExtDep (InstalledDependency dep) = disp dep

    isInstalled = not (null (installedVersions pkginfo))
    hasExes = length (executables pkginfo) >= 2
    --TODO: exclude non-buildable exes
    pkgkind | hasLib pkginfo && hasExes        = text "programs and library"
            | hasLib pkginfo && hasExe pkginfo = text "program and library"
            | hasLib pkginfo                   = text "library"
            | hasExes                          = text "programs"
            | hasExe pkginfo                   = text "program"
            | otherwise                        = empty


reflowParagraphs :: String -> Doc
reflowParagraphs =
    vcat
  . intersperse (text "")                    -- re-insert blank lines
  . map (fsep . map text . concatMap words)  -- reflow paragraphs
  . filter (/= [""])
  . groupBy (\x y -> "" `notElem` [x,y])     -- break on blank lines
  . lines

reflowLines :: String -> Doc
reflowLines = vcat . map text . lines

-- | We get the 'PackageDisplayInfo' by combining the info for the installed
-- and available versions of a package.
--
-- * We're building info about a various versions of a single named package so
-- the input package info records are all supposed to refer to the same
-- package name.
--
mergePackageInfo :: VersionRange
                 -> [Installed.InstalledPackageInfo]
                 -> [UnresolvedSourcePackage]
                 -> Maybe UnresolvedSourcePackage
                 -> Bool
                 -> PackageDisplayInfo
mergePackageInfo versionPref installedPkgs sourcePkgs selectedPkg showVer =
  assert (length installedPkgs + length sourcePkgs > 0) $
  PackageDisplayInfo {
    pkgName           = combine packageName source
                                packageName installed,
    selectedVersion   = if showVer then fmap packageVersion selectedPkg
                                   else Nothing,
    selectedSourcePkg = sourceSelected,
    installedVersions = map packageVersion installedPkgs,
    sourceVersions    = map packageVersion sourcePkgs,
    preferredVersions = versionPref,

    license      = combine Source.license       source
                           Installed.license    installed,
    maintainer   = combine Source.maintainer    source
                           Installed.maintainer installed,
    author       = combine Source.author        source
                           Installed.author     installed,
    homepage     = combine Source.homepage      source
                           Installed.homepage   installed,
    bugReports   = maybe "" Source.bugReports source,
    sourceRepo   = fromMaybe "" . join
                 . fmap (uncons Nothing Source.repoLocation
                       . sortBy (comparing Source.repoKind)
                       . Source.sourceRepos)
                 $ source,
                    --TODO: installed package info is missing synopsis
    synopsis     = maybe "" Source.synopsis      source,
    description  = combine Source.description    source
                           Installed.description installed,
    category     = combine Source.category       source
                           Installed.category    installed,
    flags        = maybe [] Source.genPackageFlags sourceGeneric,
    hasLib       = isJust installed
                || maybe False (isJust . Source.condLibrary) sourceGeneric,
    hasExe       = maybe False (not . null . Source.condExecutables) sourceGeneric,
    executables  = map fst (maybe [] Source.condExecutables sourceGeneric),
    modules      = combine (map Installed.exposedName . Installed.exposedModules)
                           installed
                           -- NB: only for the PUBLIC library
                           (concatMap getListOfExposedModules . maybeToList . Source.library)
                           source,
    dependencies =
      combine (map (SourceDependency . simplifyDependency)
               . Source.buildDepends) source
      (map InstalledDependency . Installed.depends) installed,
    haddockHtml  = fromMaybe "" . join
                 . fmap (listToMaybe . Installed.haddockHTMLs)
                 $ installed,
    haveTarball  = False
  }
  where
    combine f x g y  = fromJust (fmap f x `mplus` fmap g y)
    installed :: Maybe Installed.InstalledPackageInfo
    installed = latestWithPref versionPref installedPkgs

    getListOfExposedModules lib = Source.exposedModules lib
                               ++ map Source.moduleReexportName
                                      (Source.reexportedModules lib)

    sourceSelected
      | isJust selectedPkg = selectedPkg
      | otherwise          = latestWithPref versionPref sourcePkgs
    sourceGeneric = fmap packageDescription sourceSelected
    source        = fmap flattenPackageDescription sourceGeneric

    uncons :: b -> (a -> b) -> [a] -> b
    uncons z _ []    = z
    uncons _ f (x:_) = f x


-- | Not all the info is pure. We have to check if the docs really are
-- installed, because the registered package info lies. Similarly we have to
-- check if the tarball has indeed been fetched.
--
updateFileSystemPackageDetails :: PackageDisplayInfo -> IO PackageDisplayInfo
updateFileSystemPackageDetails pkginfo = do
  fetched   <- maybe (return False) (isFetched . packageSource)
                     (selectedSourcePkg pkginfo)
  docsExist <- doesDirectoryExist (haddockHtml pkginfo)
  return pkginfo {
    haveTarball = fetched,
    haddockHtml = if docsExist then haddockHtml pkginfo else ""
  }

latestWithPref :: Package pkg => VersionRange -> [pkg] -> Maybe pkg
latestWithPref _    []   = Nothing
latestWithPref pref pkgs = Just (maximumBy (comparing prefThenVersion) pkgs)
  where
    prefThenVersion pkg = let ver = packageVersion pkg
                           in (withinRange ver pref, ver)


-- | Rearrange installed and source packages into groups referring to the
-- same package by name. In the result pairs, the lists are guaranteed to not
-- both be empty.
--
mergePackages :: [Installed.InstalledPackageInfo]
              -> [UnresolvedSourcePackage]
              -> [( PackageName
                  , [Installed.InstalledPackageInfo]
                  , [UnresolvedSourcePackage] )]
mergePackages installedPkgs sourcePkgs =
    map collect
  $ mergeBy (\i a -> fst i `compare` fst a)
            (groupOn packageName installedPkgs)
            (groupOn packageName sourcePkgs)
  where
    collect (OnlyInLeft  (name,is)         ) = (name, is, [])
    collect (    InBoth  (_,is)   (name,as)) = (name, is, as)
    collect (OnlyInRight          (name,as)) = (name, [], as)

groupOn :: Ord key => (a -> key) -> [a] -> [(key,[a])]
groupOn key = map (\xs -> (key (head xs), xs))
            . groupBy (equating key)
            . sortBy (comparing key)

dispTopVersions :: Int -> VersionRange -> [Version] -> Doc
dispTopVersions n pref vs =
         (Disp.fsep . Disp.punctuate (Disp.char ',')
        . map (\ver -> if ispref ver then disp ver else parens (disp ver))
        . sort . take n . interestingVersions ispref
        $ vs)
    <+> trailingMessage

  where
    ispref ver = withinRange ver pref
    extra = length vs - n
    trailingMessage
      | extra <= 0 = Disp.empty
      | otherwise  = Disp.parens $ Disp.text "and"
                               <+> Disp.int (length vs - n)
                               <+> if extra == 1 then Disp.text "other"
                                                 else Disp.text "others"

-- | Reorder a bunch of versions to put the most interesting / significant
-- versions first. A preferred version range is taken into account.
--
-- This may be used in a user interface to select a small number of versions
-- to present to the user, e.g.
--
-- > let selectVersions = sort . take 5 . interestingVersions pref
--
interestingVersions :: (Version -> Bool) -> [Version] -> [Version]
interestingVersions pref =
      map (mkVersion . fst) . filter snd
    . concat  . Tree.levels
    . swizzleTree
    . reorderTree (\(Node (v,_) _) -> pref (mkVersion v))
    . reverseTree
    . mkTree
    . map versionNumbers

  where
    swizzleTree = unfoldTree (spine [])
      where
        spine ts' (Node x [])     = (x, ts')
        spine ts' (Node x (t:ts)) = spine (Node x ts:ts') t

    reorderTree _ (Node x []) = Node x []
    reorderTree p (Node x ts) = Node x (ts' ++ ts'')
      where
        (ts',ts'') = partition p (map (reorderTree p) ts)

    reverseTree (Node x cs) = Node x (reverse (map reverseTree cs))

    mkTree xs = unfoldTree step (False, [], xs)
      where
        step (node,ns,vs) =
          ( (reverse ns, node)
          , [ (any null vs', n:ns, filter (not . null) vs')
            | (n, vs') <- groups vs ]
          )
        groups = map (\g -> (head (head g), map tail g))
               . groupBy (equating head)
