module HaddockModuleTree ( ModuleTree(..), mkModuleTree ) where

import HaddockTypes  ( DocName )
import GHC           ( HsDoc, Name )
import Module        ( Module, moduleNameString, moduleName, modulePackageId )
import PackageConfig ( packageIdString )

data ModuleTree = Node String Bool (Maybe String) (Maybe (HsDoc Name)) [ModuleTree]

mkModuleTree :: Bool -> [(Module, Maybe (HsDoc Name))] -> [ModuleTree]
mkModuleTree showPkgs mods = 
  foldr fn [] [ (splitModule mod, modPkg mod, short) | (mod, short) <- mods ]
  where
    modPkg mod | showPkgs = Just (packageIdString (modulePackageId mod))
               | otherwise = Nothing
    fn (mod,pkg,short) trees = addToTrees mod pkg short trees

addToTrees :: [String] -> Maybe String -> Maybe (HsDoc Name) -> [ModuleTree] -> [ModuleTree]
addToTrees [] pkg short ts = ts
addToTrees ss pkg short [] = mkSubTree ss pkg short
addToTrees (s1:ss) pkg short (t@(Node s2 leaf node_pkg node_short subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) pkg short ts
  | s1 == s2  = Node s2 (leaf || null ss) this_pkg this_short (addToTrees ss pkg short subs) : ts
  | otherwise = mkSubTree (s1:ss) pkg short ++ t : ts
 where
  this_pkg = if null ss then pkg else node_pkg
  this_short = if null ss then short else node_short

mkSubTree :: [String] -> Maybe String -> Maybe (HsDoc Name) -> [ModuleTree]
mkSubTree []     pkg short = []
mkSubTree [s]    pkg short = [Node s True pkg short []]
mkSubTree (s:ss) pkg short = [Node s (null ss) Nothing Nothing (mkSubTree ss pkg short)]

splitModule :: Module -> [String]
splitModule mod = split (moduleNameString (moduleName mod))
  where split mod0 = case break (== '.') mod0 of
     			(s1, '.':s2) -> s1 : split s2
     			(s1, _)      -> [s1]
