{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

-- | Utilities to help format error messages for the various CLI commands.
--
module Distribution.Client.CmdErrorMessages (
    module Distribution.Client.CmdErrorMessages,
    module Distribution.Client.TargetSelector,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetSelector
         ( ComponentKindFilter, componentKind, showTargetSelector )

import Distribution.Package
         ( packageId, packageName )
import Distribution.Types.ComponentName
         ( showComponentName )
import Distribution.Solver.Types.OptionalStanza
         ( OptionalStanza(..) )
import Distribution.Text
         ( display )

import Data.Maybe (isNothing)
import Data.List (sortBy, groupBy, nub)
import Data.Function (on)


-----------------------
-- Singular or plural
--

-- | A tag used in rendering messages to distinguish singular or plural.
--
data Plural = Singular | Plural

-- | Used to render a singular or plural version of something
--
-- > plural (listPlural theThings) "it is" "they are"
--
plural :: Plural -> a -> a -> a
plural Singular si _pl = si
plural Plural  _si  pl = pl

-- | Singular for singleton lists and plural otherwise.
--
listPlural :: [a] -> Plural
listPlural [_] = Singular
listPlural  _  = Plural


--------------------
-- Rendering lists
--

-- | Render a list of things in the style @foo, bar and baz@
renderListCommaAnd :: [String] -> String
renderListCommaAnd []     = ""
renderListCommaAnd [x]    = x
renderListCommaAnd [x,x'] = x ++ " and " ++ x'
renderListCommaAnd (x:xs) = x ++ ", " ++ renderListCommaAnd xs

-- | Render a list of things in the style @blah blah; this that; and the other@
renderListSemiAnd :: [String] -> String
renderListSemiAnd []     = ""
renderListSemiAnd [x]    = x
renderListSemiAnd [x,x'] = x ++ "; and " ++ x'
renderListSemiAnd (x:xs) = x ++ "; " ++ renderListSemiAnd xs

-- | When rendering lists of things it often reads better to group related
-- things, e.g. grouping components by package name
--
-- > renderListSemiAnd
-- >   [     "the package " ++ display pkgname ++ " components "
-- >      ++ renderListCommaAnd showComponentName components
-- >   | (pkgname, components) <- sortGroupOn packageName allcomponents ]
--
sortGroupOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortGroupOn key = map (\xs@(x:_) -> (key x, xs))
                . groupBy ((==) `on` key)
                . sortBy  (compare `on` key)


----------------------------------------------------
-- Renderering for a few project and package types
--

renderTargetSelector :: TargetSelector PackageId -> String
renderTargetSelector (TargetPackage _ pkgid Nothing) =
    "the package " ++ display pkgid

renderTargetSelector (TargetPackage _ pkgid (Just kfilter)) =
    "the " ++ renderComponentKind Plural kfilter
 ++ " in the package " ++ display pkgid

renderTargetSelector (TargetAllPackages Nothing) =
    "all the packages in the project"

renderTargetSelector (TargetAllPackages (Just kfilter)) =
    "all the " ++ renderComponentKind Plural kfilter
 ++ " in the project"

renderTargetSelector (TargetComponent pkgid CLibName WholeComponent) =
    "the library in the package " ++ display pkgid

renderTargetSelector (TargetComponent _pkgid cname WholeComponent) =
    "the " ++ showComponentName cname

renderTargetSelector (TargetComponent _pkgid cname (FileTarget filename)) =
    "the file " ++ filename ++ " in the " ++ showComponentName cname

renderTargetSelector (TargetComponent _pkgid cname (ModuleTarget modname)) =
    "the module " ++ display modname ++ " in the " ++ showComponentName cname

renderTargetSelector (TargetPackageName pkgname) =
    "the package " ++ display pkgname


renderOptionalStanza :: Plural -> OptionalStanza -> String
renderOptionalStanza Singular TestStanzas  = "test suite"
renderOptionalStanza Plural   TestStanzas  = "test suites"
renderOptionalStanza Singular BenchStanzas = "benchmark"
renderOptionalStanza Plural   BenchStanzas = "benchmarks"

-- | The optional stanza type (test suite or benchmark), if it is one.
optionalStanza :: ComponentName -> Maybe OptionalStanza
optionalStanza (CTestName  _) = Just TestStanzas
optionalStanza (CBenchName _) = Just BenchStanzas
optionalStanza _              = Nothing

-- | Does the 'TargetSelector' potentially refer to one package or many?
--
targetSelectorPluralPkgs :: TargetSelector a -> Plural
targetSelectorPluralPkgs (TargetAllPackages _)     = Plural
targetSelectorPluralPkgs (TargetPackage _ _ _)     = Singular
targetSelectorPluralPkgs (TargetComponent _ _ _)   = Singular
targetSelectorPluralPkgs (TargetPackageName _)     = Singular

-- | Does the 'TargetSelector' refer to 
targetSelectorRefersToPkgs :: TargetSelector a -> Bool
targetSelectorRefersToPkgs (TargetAllPackages  mkfilter) = isNothing mkfilter
targetSelectorRefersToPkgs (TargetPackage  _ _ mkfilter) = isNothing mkfilter
targetSelectorRefersToPkgs (TargetComponent _ _ _)       = False
targetSelectorRefersToPkgs (TargetPackageName _)         = True

targetSelectorFilter :: TargetSelector a -> Maybe ComponentKindFilter
targetSelectorFilter (TargetPackage  _ _ mkfilter) = mkfilter
targetSelectorFilter (TargetAllPackages  mkfilter) = mkfilter
targetSelectorFilter (TargetComponent _ _ _)       = Nothing
targetSelectorFilter (TargetPackageName _)         = Nothing

renderComponentKind :: Plural -> ComponentKind -> String
renderComponentKind Singular ckind = case ckind of
  LibKind   -> "library"  -- internal/sub libs?
  FLibKind  -> "foreign library"
  ExeKind   -> "executable"
  TestKind  -> "test suite"
  BenchKind -> "benchmark"
renderComponentKind Plural ckind = case ckind of
  LibKind   -> "libraries"  -- internal/sub libs?
  FLibKind  -> "foreign libraries"
  ExeKind   -> "executables"
  TestKind  -> "test suites"
  BenchKind -> "benchmarks"


-------------------------------------------------------
-- Renderering error messages for TargetProblemCommon
--

renderTargetProblemCommon :: String -> TargetProblemCommon -> String
renderTargetProblemCommon verb (TargetNotInProject pkgname) =
    "Cannot " ++ verb ++ " the package " ++ display pkgname ++ ", it is not "
 ++ "in this project (either directly or indirectly). If you want to add it "
 ++ "to the project then edit the cabal.project file."

renderTargetProblemCommon verb (TargetComponentNotProjectLocal pkgid cname _) =
    "Cannot " ++ verb ++ " the " ++ showComponentName cname ++ " because the "
 ++ "package " ++ display pkgid ++ " is not local to the project, and cabal "
 ++ "does not currently support building test suites or benchmarks of "
 ++ "non-local dependencies. To run test suites or benchmarks from "
 ++ "dependencies you can unpack the package locally and adjust the "
 ++ "cabal.project file to include that package directory."

renderTargetProblemCommon verb (TargetComponentNotBuildable pkgid cname _) =
    "Cannot " ++ verb ++ " the " ++ showComponentName cname ++ " because it is "
 ++ "marked as 'buildable: False' within the '" ++ display (packageName pkgid)
 ++ ".cabal' file (at least for the current configuration). If you believe it "
 ++ "should be buildable then check the .cabal file to see if the buildable "
 ++ "property is conditional on flags. Alternatively you may simply have to "
 ++ "edit the .cabal file to declare it as buildable and fix any resulting "
 ++ "build problems."

renderTargetProblemCommon verb (TargetOptionalStanzaDisabledByUser _ cname _) =
    "Cannot " ++ verb ++ " the " ++ showComponentName cname ++ " because "
 ++ "building " ++ compkinds ++ " has been explicitly disabled in the "
 ++ "configuration. You can adjust this configuration in the "
 ++ "cabal.project{.local} file either for all packages in the project or on "
 ++ "a per-package basis. Note that if you do not explicitly disable "
 ++ compkinds ++ " then the solver will merely try to make a plan with "
 ++ "them available, so you may wish to explicitly enable them which will "
 ++ "require the solver to find a plan with them available or to fail with an "
 ++ "explanation."
   where
     compkinds = renderComponentKind Plural (componentKind cname)

renderTargetProblemCommon verb (TargetOptionalStanzaDisabledBySolver pkgid cname _) =
    "Cannot " ++ verb ++ " the " ++ showComponentName cname ++ " because the "
 ++ "solver did not find a plan that included the " ++ compkinds
 ++ " for " ++ display pkgid ++ ". It is probably worth trying again with "
 ++ compkinds ++ "explicitly enabled in the configuration in the "
 ++ "cabal.project{.local} file. This will ask the solver to find a plan with "
 ++ "the " ++ compkinds ++ " available. It will either fail with an "
 ++ "explanation or find a different plan that uses different versions of some "
 ++ "other packages. Use the '--dry-run' flag to see package versions and "
 ++ "check that you are happy with the choices."
   where
     compkinds = renderComponentKind Plural (componentKind cname)

renderTargetProblemCommon verb (TargetProblemNoSuchPackage pkgid) =
    "Internal error when trying to " ++ verb ++ " the package "
  ++ display pkgid ++ ". The package is not in the set of available targets "
  ++ "for the project plan, which would suggest an inconsistency "
  ++ "between readTargetSelectors and resolveTargets."

renderTargetProblemCommon verb (TargetProblemNoSuchComponent pkgid cname) =
    "Internal error when trying to " ++ verb ++ " the "
  ++ showComponentName cname ++ " from the package " ++ display pkgid
  ++ ". The package,component pair is not in the set of available targets "
  ++ "for the project plan, which would suggest an inconsistency "
  ++ "between readTargetSelectors and resolveTargets."


------------------------------------------------------------
-- Renderering error messages for TargetProblemNoneEnabled
--

-- | Several commands have a @TargetProblemNoneEnabled@ problem constructor.
-- This renders an error message for those cases.
--
renderTargetProblemNoneEnabled :: String
                               -> TargetSelector PackageId
                               -> [AvailableTarget ()]
                               -> String
renderTargetProblemNoneEnabled verb targetSelector targets =
    "Cannot " ++ verb ++ " " ++ renderTargetSelector targetSelector
 ++ " because none of the components are available to build: "
 ++ renderListSemiAnd
    [ case (status, mstanza) of
        (TargetDisabledByUser, Just stanza) ->
            renderListCommaAnd
              [ "the " ++ showComponentName availableTargetComponentName
              | AvailableTarget {availableTargetComponentName} <- targets' ]
         ++ plural (listPlural targets') " is " " are "
         ++ " not available because building "
         ++ renderOptionalStanza Plural stanza
         ++ " has been disabled in the configuration"
        (TargetDisabledBySolver, Just stanza) ->
            renderListCommaAnd
              [ "the " ++ showComponentName availableTargetComponentName
              | AvailableTarget {availableTargetComponentName} <- targets' ]
         ++ plural (listPlural targets') " is " " are "
         ++ "not available because the solver did not find a plan that "
         ++ "included the " ++ renderOptionalStanza Plural stanza
        (TargetNotBuildable, _) ->
            renderListCommaAnd
              [ "the " ++ showComponentName availableTargetComponentName
              | AvailableTarget {availableTargetComponentName} <- targets' ]
         ++ plural (listPlural targets') " is " " are all "
         ++ "marked as 'buildable: False'"
        (TargetNotLocal, _) ->
            renderListCommaAnd
              [ "the " ++ showComponentName availableTargetComponentName
              | AvailableTarget {availableTargetComponentName} <- targets' ]
         ++ " cannot be built because cabal does not currently support "
         ++ "building test suites or benchmarks of non-local dependencies"
        (TargetBuildable () TargetNotRequestedByDefault, Just stanza) ->
            renderListCommaAnd
              [ "the " ++ showComponentName availableTargetComponentName
              | AvailableTarget {availableTargetComponentName} <- targets' ]
         ++ " will not be built because " ++ renderOptionalStanza Plural stanza
         ++ " are not built by default in the current configuration (but you "
         ++ "can still build them specifically)" --TODO: say how
        _ -> error $ "renderBuildTargetProblem: unexpected status "
                  ++ show (status, mstanza)
    | ((status, mstanza), targets') <- sortGroupOn groupingKey targets
    ]
  where
    groupingKey t =
      ( availableTargetStatus t
      , case availableTargetStatus t of
          TargetNotBuildable -> Nothing
          TargetNotLocal     -> Nothing
          _ -> optionalStanza (availableTargetComponentName t)
      )

------------------------------------------------------------
-- Renderering error messages for TargetProblemNoneEnabled
--

-- | Several commands have a @TargetProblemNoTargets@ problem constructor.
-- This renders an error message for those cases.
--
renderTargetProblemNoTargets :: String -> TargetSelector PackageId -> String
renderTargetProblemNoTargets verb targetSelector =
    "Cannot " ++ verb ++ " " ++ renderTargetSelector targetSelector
 ++ " because " ++ reason targetSelector ++ ". "
 ++ "Check the .cabal "
 ++ plural (targetSelectorPluralPkgs targetSelector)
      "file for the package and make sure that it properly declares "
      "files for the packages and make sure that they properly declare "
 ++ "the components that you expect."
  where
    reason (TargetPackage _ _ Nothing) =
        "it does not contain any components at all"
    reason (TargetPackage _ _ (Just kfilter)) =
        "it does not contain any " ++ renderComponentKind Plural kfilter
    reason (TargetAllPackages Nothing) =
        "none of them contain any components at all"
    reason (TargetAllPackages (Just kfilter)) =
        "none of the packages contain any "
     ++ renderComponentKind Plural kfilter
    reason ts@TargetComponent{} =
        error $ "renderTargetProblemNoTargets: " ++ show ts
    reason (TargetPackageName _) =
        "it does not contain any components at all"

-----------------------------------------------------------
-- Renderering error messages for CannotPruneDependencies
--

renderCannotPruneDependencies :: CannotPruneDependencies -> String
renderCannotPruneDependencies (CannotPruneDependencies brokenPackages) =
      "Cannot select only the dependencies (as requested by the "
   ++ "'--only-dependencies' flag), "
   ++ (case pkgids of
          [pkgid] -> "the package " ++ display pkgid ++ " is "
          _       -> "the packages "
                     ++ renderListCommaAnd (map display pkgids) ++ " are ")
   ++ "required by a dependency of one of the other targets."
  where
    -- throw away the details and just list the deps that are needed
    pkgids :: [PackageId]
    pkgids = nub . map packageId . concatMap snd $ brokenPackages

{-
           ++ "Syntax:\n"
           ++ " - build [package]\n"
           ++ " - build [package:]component\n"
           ++ " - build [package:][component:]module\n"
           ++ " - build [package:][component:]file\n"
           ++ " where\n"
           ++ "  package is a package name, package dir or .cabal file\n\n"
           ++ "Examples:\n"
           ++ " - build foo            -- package name\n"
           ++ " - build tests          -- component name\n"
           ++ "    (name of library, executable, test-suite or benchmark)\n"
           ++ " - build Data.Foo       -- module name\n"
           ++ " - build Data/Foo.hsc   -- file name\n\n"
           ++ "An ambigious target can be qualified by package, component\n"
           ++ "and/or component kind (lib|exe|test|bench|flib)\n"
           ++ " - build foo:tests      -- component qualified by package\n"
           ++ " - build tests:Data.Foo -- module qualified by component\n"
           ++ " - build lib:foo        -- component qualified by kind"
-}
