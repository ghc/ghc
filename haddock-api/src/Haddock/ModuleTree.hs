-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.ModuleTree
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.ModuleTree ( ModuleTree(..), mkModuleTree ) where


import Haddock.Types ( MDoc )

import GHC           ( Name )
import Module        ( Module, moduleNameString, moduleName, moduleUnitId, unitIdString )
import DynFlags      ( DynFlags )
import Packages      ( lookupPackage )
import PackageConfig ( sourcePackageIdString )


data ModuleTree = Node String Bool (Maybe String) (Maybe String) (Maybe (MDoc Name)) [ModuleTree]


mkModuleTree :: DynFlags -> Bool -> [(Module, Maybe (MDoc Name))] -> [ModuleTree]
mkModuleTree dflags showPkgs mods =
  foldr fn [] [ (splitModule mdl, modPkg mdl, modSrcPkg mdl, short) | (mdl, short) <- mods ]
  where
    modPkg mod_ | showPkgs = Just (unitIdString (moduleUnitId mod_))
                | otherwise = Nothing
    modSrcPkg mod_ | showPkgs = fmap sourcePackageIdString
                                     (lookupPackage dflags (moduleUnitId mod_))
                   | otherwise = Nothing
    fn (mod_,pkg,srcPkg,short) = addToTrees mod_ pkg srcPkg short


addToTrees :: [String] -> Maybe String -> Maybe String -> Maybe (MDoc Name) -> [ModuleTree] -> [ModuleTree]
addToTrees [] _ _ _ ts = ts
addToTrees ss pkg srcPkg short [] = mkSubTree ss pkg srcPkg short
addToTrees (s1:ss) pkg srcPkg short (t@(Node s2 leaf node_pkg node_srcPkg node_short subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) pkg srcPkg short ts
  | s1 == s2  = Node s2 (leaf || null ss) this_pkg this_srcPkg this_short (addToTrees ss pkg srcPkg short subs) : ts
  | otherwise = mkSubTree (s1:ss) pkg srcPkg short ++ t : ts
 where
  this_pkg = if null ss then pkg else node_pkg
  this_srcPkg = if null ss then srcPkg else node_srcPkg
  this_short = if null ss then short else node_short


mkSubTree :: [String] -> Maybe String -> Maybe String -> Maybe (MDoc Name) -> [ModuleTree]
mkSubTree []     _   _      _     = []
mkSubTree [s]    pkg srcPkg short = [Node s True pkg srcPkg short []]
mkSubTree (s:ss) pkg srcPkg short = [Node s (null ss) Nothing Nothing Nothing (mkSubTree ss pkg srcPkg short)]


splitModule :: Module -> [String]
splitModule mdl = split (moduleNameString (moduleName mdl))
  where split mod0 = case break (== '.') mod0 of
          (s1, '.':s2) -> s1 : split s2
          (s1, _)      -> [s1]
