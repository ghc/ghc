{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Backpack.PreModuleShape (
    PreModuleShape(..),
    toPreModuleShape,
    renamePreModuleShape,
    mixLinkPreModuleShape,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Distribution.Backpack.ModuleShape
import Distribution.Types.IncludeRenaming
import Distribution.Types.ModuleRenaming
import Distribution.ModuleName

data PreModuleShape = PreModuleShape {
        preModShapeProvides :: Set ModuleName,
        preModShapeRequires :: Set ModuleName
    }
    deriving (Eq, Show, Generic)

toPreModuleShape :: ModuleShape -> PreModuleShape
toPreModuleShape (ModuleShape provs reqs) = PreModuleShape (Map.keysSet provs) reqs

renamePreModuleShape :: PreModuleShape -> IncludeRenaming -> PreModuleShape
renamePreModuleShape (PreModuleShape provs reqs) (IncludeRenaming prov_rn req_rn) =
    PreModuleShape
        (Set.fromList (mapMaybe prov_fn (Set.toList provs)))
        (Set.map req_fn reqs)
  where
    prov_fn = interpModuleRenaming prov_rn
    req_fn k = fromMaybe k (interpModuleRenaming req_rn k)

mixLinkPreModuleShape :: [PreModuleShape] -> PreModuleShape
mixLinkPreModuleShape shapes = PreModuleShape provs (Set.difference reqs provs)
  where
    provs = Set.unions (map preModShapeProvides shapes)
    reqs  = Set.unions (map preModShapeRequires shapes)
