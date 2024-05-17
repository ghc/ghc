-----------------------------------------------------------------------------

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
module Haddock.ModuleTree (ModuleTree (..), mkModuleTree) where

import Haddock.Types (MDoc)

import GHC (Name)
import GHC.Unit.Module (Module, moduleName, moduleNameString, moduleUnit, unitString)
import GHC.Unit.State (UnitState, lookupUnit, unitPackageIdString)

import qualified Control.Applicative as A

data ModuleTree = Node String (Maybe Module) (Maybe String) (Maybe String) (Maybe (MDoc Name)) [ModuleTree]

mkModuleTree :: UnitState -> Bool -> [(Module, Maybe (MDoc Name))] -> [ModuleTree]
mkModuleTree state showPkgs mods =
  foldr fn [] [(mdl, splitModule mdl, modPkg mdl, modSrcPkg mdl, short) | (mdl, short) <- mods]
  where
    modPkg mod_
      | showPkgs = Just (unitString (moduleUnit mod_))
      | otherwise = Nothing
    modSrcPkg mod_
      | showPkgs =
          fmap
            unitPackageIdString
            (lookupUnit state (moduleUnit mod_))
      | otherwise = Nothing
    fn (m, mod_, pkg, srcPkg, short) = addToTrees mod_ m pkg srcPkg short

addToTrees :: [String] -> Module -> Maybe String -> Maybe String -> Maybe (MDoc Name) -> [ModuleTree] -> [ModuleTree]
addToTrees [] _ _ _ _ ts = ts
addToTrees ss m pkg srcPkg short [] = mkSubTree ss m pkg srcPkg short
addToTrees (s1 : ss) m pkg srcPkg short (t@(Node s2 leaf node_pkg node_srcPkg node_short subs) : ts)
  | s1 > s2 = t : addToTrees (s1 : ss) m pkg srcPkg short ts
  | s1 == s2 = Node s2 (leaf A.<|> (if null ss then Just m else Nothing)) this_pkg this_srcPkg this_short (addToTrees ss m pkg srcPkg short subs) : ts
  | otherwise = mkSubTree (s1 : ss) m pkg srcPkg short ++ t : ts
  where
    this_pkg = if null ss then pkg else node_pkg
    this_srcPkg = if null ss then srcPkg else node_srcPkg
    this_short = if null ss then short else node_short

mkSubTree :: [String] -> Module -> Maybe String -> Maybe String -> Maybe (MDoc Name) -> [ModuleTree]
mkSubTree [] _ _ _ _ = []
mkSubTree [s] m pkg srcPkg short = [Node s (Just m) pkg srcPkg short []]
mkSubTree (s : s' : ss) m pkg srcPkg short = [Node s Nothing Nothing Nothing Nothing (mkSubTree (s' : ss) m pkg srcPkg short)]

splitModule :: Module -> [String]
splitModule mdl = split (moduleNameString (moduleName mdl))
  where
    split mod0 = case break (== '.') mod0 of
      (s1, '.' : s2) -> s1 : split s2
      (s1, _) -> [s1]
