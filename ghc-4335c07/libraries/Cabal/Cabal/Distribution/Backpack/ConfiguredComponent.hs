{-# LANGUAGE PatternGuards #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ConfiguredComponent (
    ConfiguredComponent(..),
    cc_name,
    cc_cid,
    cc_pkgid,
    toConfiguredComponent,
    toConfiguredComponents,
    dispConfiguredComponent,

    ConfiguredComponentMap,
    extendConfiguredComponentMap,

    -- TODO: Should go somewhere else
    newPackageDepsBehaviour
) where

import Prelude ()
import Distribution.Compat.Prelude hiding ((<>))

import Distribution.Backpack.Id

import Distribution.Types.AnnotatedId
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.IncludeRenaming
import Distribution.Types.ComponentId
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Mixin
import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Types.ComponentInclude
import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.Setup as Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Version
import Distribution.Utils.LogProgress
import Distribution.Utils.MapAccum

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import Distribution.Text
import Text.PrettyPrint

-- | A configured component, we know exactly what its 'ComponentId' is,
-- and the 'ComponentId's of the things it depends on.
data ConfiguredComponent
    = ConfiguredComponent {
        -- | Unique identifier of component, plus extra useful info.
        cc_ann_id :: AnnotatedId ComponentId,
        -- | The fragment of syntax from the Cabal file describing this
        -- component.
        cc_component :: Component,
        -- | Is this the public library component of the package?
        -- (If we invoke Setup with an instantiation, this is the
        -- component the instantiation applies to.)
        -- Note that in one-component configure mode, this is
        -- always True, because any component is the "public" one.)
        cc_public :: Bool,
        -- | Dependencies on executables from @build-tools@ and
        -- @build-tool-depends@.
        cc_exe_deps :: [AnnotatedId ComponentId],
        -- | The mixins of this package, including both explicit (from
        -- the @mixins@ field) and implicit (from @build-depends@).  Not
        -- mix-in linked yet; component configuration only looks at
        -- 'ComponentId's.
        cc_includes :: [ComponentInclude ComponentId IncludeRenaming]
      }


-- | Uniquely identifies a configured component.
cc_cid :: ConfiguredComponent -> ComponentId
cc_cid = ann_id . cc_ann_id

-- | The package this component came from.
cc_pkgid :: ConfiguredComponent -> PackageId
cc_pkgid = ann_pid . cc_ann_id

-- | The 'ComponentName' of a component; this uniquely identifies
-- a fragment of syntax within a specified Cabal file describing the
-- component.
cc_name :: ConfiguredComponent -> ComponentName
cc_name = ann_cname . cc_ann_id

-- | Pretty-print a 'ConfiguredComponent'.
dispConfiguredComponent :: ConfiguredComponent -> Doc
dispConfiguredComponent cc =
    hang (text "component" <+> disp (cc_cid cc)) 4
         (vcat [ hsep $ [ text "include", disp (ci_id incl), disp (ci_renaming incl) ]
               | incl <- cc_includes cc
               ])

-- | Construct a 'ConfiguredComponent', given that the 'ComponentId'
-- and library/executable dependencies are known.  The primary
-- work this does is handling implicit @backpack-include@ fields.
mkConfiguredComponent
    :: PackageDescription
    -> ComponentId
    -> [AnnotatedId ComponentId] -- lib deps
    -> [AnnotatedId ComponentId] -- exe deps
    -> Component
    -> LogProgress ConfiguredComponent
mkConfiguredComponent pkg_descr this_cid lib_deps exe_deps component = do
    -- Resolve each @mixins@ into the actual dependency
    -- from @lib_deps@.
    explicit_includes <- forM (mixins bi) $ \(Mixin name rns) -> do
        let keys = fixFakePkgName pkg_descr name
        aid <- case Map.lookup keys deps_map of
                Nothing ->
                    dieProgress $
                        text "Mix-in refers to non-existent package" <+>
                        quotes (disp name) $$
                        text "(did you forget to add the package to build-depends?)"
                Just r  -> return r
        return ComponentInclude {
                ci_ann_id   = aid,
                ci_renaming = rns,
                ci_implicit = False
            }

        -- Any @build-depends@ which is not explicitly mentioned in
        -- @backpack-include@ is converted into an "implicit" include.
    let used_explicitly = Set.fromList (map ci_id explicit_includes)
        implicit_includes
            = map (\aid -> ComponentInclude {
                                ci_ann_id = aid,
                                ci_renaming = defaultIncludeRenaming,
                                ci_implicit = True
                            })
            $ filter (flip Set.notMember used_explicitly . ann_id) lib_deps

    return ConfiguredComponent {
            cc_ann_id = AnnotatedId {
                    ann_id = this_cid,
                    ann_pid = package pkg_descr,
                    ann_cname = componentName component
                },
            cc_component = component,
            cc_public = is_public,
            cc_exe_deps = exe_deps,
            cc_includes = explicit_includes ++ implicit_includes
        }
  where
    bi = componentBuildInfo component
    deps_map = Map.fromList [ ((packageName dep, ann_cname dep), dep)
                            | dep <- lib_deps ]
    is_public = componentName component == CLibName

type ConfiguredComponentMap =
        Map PackageName (Map ComponentName (AnnotatedId ComponentId))

toConfiguredComponent
    :: PackageDescription
    -> ComponentId
    -> ConfiguredComponentMap
    -> Component
    -> LogProgress ConfiguredComponent
toConfiguredComponent pkg_descr this_cid dep_map component = do
    lib_deps <-
        if newPackageDepsBehaviour pkg_descr
            then forM (targetBuildDepends bi) $ \(Dependency name _) -> do
                    let (pn, cn) = fixFakePkgName pkg_descr name
                    value <- case Map.lookup cn =<< Map.lookup pn dep_map of
                        Nothing ->
                            dieProgress $
                                text "Dependency on unbuildable" <+>
                                text (showComponentName cn) <+>
                                text "from" <+> disp pn
                        Just v -> return v
                    return value
            else return old_style_lib_deps
    mkConfiguredComponent
       pkg_descr this_cid
       lib_deps exe_deps component
  where
    bi = componentBuildInfo component
    -- dep_map contains a mix of internal and external deps.
    -- We want all the public libraries (dep_cn == CLibName)
    -- of all external deps (dep /= pn).  Note that this
    -- excludes the public library of the current package:
    -- this is not supported by old-style deps behavior
    -- because it would imply a cyclic dependency for the
    -- library itself.
    old_style_lib_deps = [ e
                         | (pn, comp_map) <- Map.toList dep_map
                         , pn /= packageName pkg_descr
                         , (cn, e) <- Map.toList comp_map
                         , cn == CLibName ]
    exe_deps =
        [ exe
        | ExeDependency pn cn _ <- getAllToolDependencies pkg_descr bi
        -- The error suppression here is important, because in general
        -- we won't know about external dependencies (e.g., 'happy')
        -- which the package is attempting to use (those deps are only
        -- fed in when cabal-install uses this codepath.)
        -- TODO: Let cabal-install request errors here
        , Just exe <- [Map.lookup (CExeName cn) =<< Map.lookup pn dep_map]
        ]

-- | Also computes the 'ComponentId', and sets cc_public if necessary.
-- This is Cabal-only; cabal-install won't use this.
toConfiguredComponent'
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> PackageDescription
    -> Bool -- deterministic
    -> Flag String      -- configIPID (todo: remove me)
    -> Flag ComponentId -- configCID
    -> ConfiguredComponentMap
    -> Component
    -> LogProgress ConfiguredComponent
toConfiguredComponent' use_external_internal_deps flags
                pkg_descr deterministic ipid_flag cid_flag
                dep_map component = do
    cc <- toConfiguredComponent
                pkg_descr this_cid
                dep_map component
    return $ if use_external_internal_deps
                then cc { cc_public = True }
                else cc
  where
    -- TODO: pass component names to it too!
    this_cid = computeComponentId deterministic ipid_flag cid_flag (package pkg_descr)
                (componentName component) (Just (deps, flags))
    deps = [ ann_id aid | m <- Map.elems dep_map
                        , aid <- Map.elems m ]

extendConfiguredComponentMap
    :: ConfiguredComponent
    -> ConfiguredComponentMap
    -> ConfiguredComponentMap
extendConfiguredComponentMap cc =
    Map.insertWith Map.union
        (pkgName (cc_pkgid cc))
        (Map.singleton (cc_name cc) (cc_ann_id cc))

-- Compute the 'ComponentId's for a graph of 'Component's.  The
-- list of internal components must be topologically sorted
-- based on internal package dependencies, so that any internal
-- dependency points to an entry earlier in the list.
toConfiguredComponents
    :: Bool -- use_external_internal_deps
    -> FlagAssignment
    -> Bool -- deterministic
    -> Flag String -- configIPID
    -> Flag ComponentId -- configCID
    -> PackageDescription
    -> ConfiguredComponentMap
    -> [Component]
    -> LogProgress [ConfiguredComponent]
toConfiguredComponents
    use_external_internal_deps flags deterministic ipid_flag cid_flag pkg_descr
    dep_map comps
    = fmap snd (mapAccumM go dep_map comps)
  where
    go m component = do
        cc <- toConfiguredComponent'
                        use_external_internal_deps flags pkg_descr
                        deterministic ipid_flag cid_flag
                        m component
        return (extendConfiguredComponentMap cc m, cc)

newPackageDepsBehaviourMinVersion :: Version
newPackageDepsBehaviourMinVersion = mkVersion [1,7,1]


-- In older cabal versions, there was only one set of package dependencies for
-- the whole package. In this version, we can have separate dependencies per
-- target, but we only enable this behaviour if the minimum cabal version
-- specified is >= a certain minimum. Otherwise, for compatibility we use the
-- old behaviour.
newPackageDepsBehaviour :: PackageDescription -> Bool
newPackageDepsBehaviour pkg =
   specVersion pkg >= newPackageDepsBehaviourMinVersion

-- | 'build-depends:' stanzas are currently ambiguous as the external packages
-- and internal libraries are specified the same. For now, we assume internal
-- libraries shadow, and this function disambiguates accordingly, but soon the
-- underlying ambiguity will be addressed.
fixFakePkgName :: PackageDescription -> PackageName -> (PackageName, ComponentName)
fixFakePkgName pkg_descr pn =
  if subLibName `elem` internalLibraries
  then (packageName pkg_descr, CSubLibName subLibName)
  else (pn,                    CLibName)
  where
    subLibName = packageNameToUnqualComponentName pn
    internalLibraries = mapMaybe libName (allLibraries pkg_descr)
