module HaddockModuleTree(ModuleTree(..), mkModuleTree) where

import HsSyn

data ModuleTree = Node String Bool (Maybe String) [ModuleTree]

mkModuleTree :: [(Module,Maybe String)] -> [ModuleTree]
mkModuleTree mods = 
  foldr fn [] [ (splitModule mod, pkg) | (mod,pkg) <- mods ]
  where 
    fn (mod,pkg) trees = addToTrees mod pkg trees

addToTrees :: [String] -> Maybe String -> [ModuleTree] -> [ModuleTree]
addToTrees [] pkg ts = ts
addToTrees ss pkg [] = mkSubTree ss pkg
addToTrees (s1:ss) pkg (t@(Node s2 leaf node_pkg subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) pkg ts
  | s1 == s2  = Node s2 (leaf || null ss) this_pkg (addToTrees ss pkg subs) : ts
  | otherwise = mkSubTree (s1:ss) pkg ++ t : ts
 where
  this_pkg = if null ss then pkg else node_pkg

mkSubTree :: [String] -> Maybe String -> [ModuleTree]
mkSubTree []     pkg = []
mkSubTree [s]    pkg = [Node s True pkg []]
mkSubTree (s:ss) pkg = [Node s (null ss) Nothing (mkSubTree ss pkg)]

splitModule :: Module -> [String]
splitModule (Module mdl) = split mdl
  where split mdl0 = case break (== '.') mdl0 of
     			(s1, '.':s2) -> s1 : split s2
     			(s1, _)      -> [s1]
