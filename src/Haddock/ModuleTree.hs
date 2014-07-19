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


import Haddock.Types ( Doc )

import GHC           ( Name )
import Module        ( Module, moduleNameString, moduleName, modulePackageKey,
                       packageKeyString )


data ModuleTree = Node String Bool (Maybe String) (Maybe (Doc Name)) [ModuleTree]


mkModuleTree :: Bool -> [(Module, Maybe (Doc Name))] -> [ModuleTree]
mkModuleTree showPkgs mods =
  foldr fn [] [ (splitModule mdl, modPkg mdl, short) | (mdl, short) <- mods ]
  where
    modPkg mod_ | showPkgs = Just (packageKeyString (modulePackageKey mod_))
                | otherwise = Nothing
    fn (mod_,pkg,short) = addToTrees mod_ pkg short


addToTrees :: [String] -> Maybe String -> Maybe (Doc Name) -> [ModuleTree] -> [ModuleTree]
addToTrees [] _ _ ts = ts
addToTrees ss pkg short [] = mkSubTree ss pkg short
addToTrees (s1:ss) pkg short (t@(Node s2 leaf node_pkg node_short subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) pkg short ts
  | s1 == s2  = Node s2 (leaf || null ss) this_pkg this_short (addToTrees ss pkg short subs) : ts
  | otherwise = mkSubTree (s1:ss) pkg short ++ t : ts
 where
  this_pkg = if null ss then pkg else node_pkg
  this_short = if null ss then short else node_short


mkSubTree :: [String] -> Maybe String -> Maybe (Doc Name) -> [ModuleTree]
mkSubTree []     _   _     = []
mkSubTree [s]    pkg short = [Node s True pkg short []]
mkSubTree (s:ss) pkg short = [Node s (null ss) Nothing Nothing (mkSubTree ss pkg short)]


splitModule :: Module -> [String]
splitModule mdl = split (moduleNameString (moduleName mdl))
  where split mod0 = case break (== '.') mod0 of
          (s1, '.':s2) -> s1 : split s2
          (s1, _)      -> [s1]
