module HaddockModuleTree(ModuleTree(..), mkModuleTree) where

import HsSyn

data ModuleTree = Node String Bool [ModuleTree]

mkModuleTree :: [Module] -> [ModuleTree]
mkModuleTree mods = foldr addToTrees [] (map splitModule mods)

addToTrees :: [String] -> [ModuleTree] -> [ModuleTree]
addToTrees [] ts = ts
addToTrees ss [] = mkSubTree ss
addToTrees (s1:ss) (t@(Node s2 leaf subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) ts
  | s1 == s2  = Node s2 (leaf || null ss) (addToTrees ss subs) : ts
  | otherwise = mkSubTree (s1:ss) ++ t : ts

mkSubTree [] = []
mkSubTree (s:ss) = [Node s (null ss) (mkSubTree ss)]

splitModule :: Module -> [String]
splitModule (Module mod) = split mod
  where split mod = case break (== '.') mod of
     			(s1, '.':s2) -> s1 : split s2
     			(s1, _) -> [s1]