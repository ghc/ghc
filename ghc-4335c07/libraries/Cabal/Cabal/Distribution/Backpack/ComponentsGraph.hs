-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ComponentsGraph (
    ComponentsGraph,
    ComponentsWithDeps,
    mkComponentsGraph,
    componentsGraphToList,
    dispComponentsWithDeps,
    componentCycleMsg
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.UnqualComponentName
import Distribution.Compat.Graph (Graph, Node(..))
import qualified Distribution.Compat.Graph as Graph

import Distribution.Text
    ( Text(disp) )
import Text.PrettyPrint

------------------------------------------------------------------------------
-- Components graph
------------------------------------------------------------------------------

-- | A graph of source-level components by their source-level
-- dependencies
--
type ComponentsGraph = Graph (Node ComponentName Component)

-- | A list of components associated with the source level
-- dependencies between them.
--
type ComponentsWithDeps = [(Component, [ComponentName])]

-- | Pretty-print 'ComponentsWithDeps'.
--
dispComponentsWithDeps :: ComponentsWithDeps -> Doc
dispComponentsWithDeps graph =
    vcat [ hang (text "component" <+> disp (componentName c)) 4
                (vcat [ text "dependency" <+> disp cdep | cdep <- cdeps ])
         | (c, cdeps) <- graph ]

-- | Create a 'Graph' of 'Component', or report a cycle if there is a
-- problem.
--
mkComponentsGraph :: ComponentRequestedSpec
                  -> PackageDescription
                  -> Either [ComponentName] ComponentsGraph
mkComponentsGraph enabled pkg_descr =
    let g = Graph.fromDistinctList
                           [ N c (componentName c) (componentDeps c)
                           | c <- pkgBuildableComponents pkg_descr
                           , componentEnabled enabled c ]
    in case Graph.cycles g of
          []     -> Right g
          ccycles -> Left  [ componentName c | N c _ _ <- concat ccycles ]
  where
    -- The dependencies for the given component
    componentDeps component =
      (CExeName <$> getAllInternalToolDependencies pkg_descr bi)

      ++ [ if pkgname == packageName pkg_descr
           then CLibName
           else CSubLibName toolname
         | Dependency pkgname _ <- targetBuildDepends bi
         , let toolname = packageNameToUnqualComponentName pkgname
         , toolname `elem` internalPkgDeps ]
      where
        bi = componentBuildInfo component
        internalPkgDeps = map (conv . libName) (allLibraries pkg_descr)
        conv Nothing = packageNameToUnqualComponentName $ packageName pkg_descr
        conv (Just s) = s

-- | Given the package description and a 'PackageDescription' (used
-- to determine if a package name is internal or not), sort the
-- components in dependency order (fewest dependencies first).  This is
-- NOT necessarily the build order (although it is in the absence of
-- Backpack.)
--
componentsGraphToList :: ComponentsGraph
                      -> ComponentsWithDeps
componentsGraphToList =
    map (\(N c _ cs) -> (c, cs)) . Graph.revTopSort

-- | Error message when there is a cycle; takes the SCC of components.
componentCycleMsg :: [ComponentName] -> Doc
componentCycleMsg cnames =
    text $ "Components in the package depend on each other in a cyclic way:\n  "
       ++ intercalate " depends on "
            [ "'" ++ showComponentName cname ++ "'"
            | cname <- cnames ++ [head cnames] ]
