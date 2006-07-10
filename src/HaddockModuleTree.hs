module HaddockModuleTree(ModuleTree(..), mkModuleTree) where

import HsSyn2

data ModuleTree = Node String Bool (Maybe String) (Maybe Doc) [ModuleTree]

mkModuleTree :: [(Module,Maybe String,Maybe Doc)] -> [ModuleTree]
mkModuleTree mods = 
  foldr fn [] [ (splitModule mod, pkg,short) | (mod,pkg,short) <- mods ]
  where 
    fn (mod,pkg,short) trees = addToTrees mod pkg short trees

addToTrees :: [String] -> Maybe String -> Maybe Doc -> [ModuleTree] -> [ModuleTree]
addToTrees [] pkg short ts = ts
addToTrees ss pkg short [] = mkSubTree ss pkg short
addToTrees (s1:ss) pkg short (t@(Node s2 leaf node_pkg node_short subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) pkg short ts
  | s1 == s2  = Node s2 (leaf || null ss) this_pkg this_short (addToTrees ss pkg short subs) : ts
  | otherwise = mkSubTree (s1:ss) pkg short ++ t : ts
 where
  this_pkg = if null ss then pkg else node_pkg
  this_short = if null ss then short else node_short

mkSubTree :: [String] -> Maybe String -> Maybe Doc -> [ModuleTree]
mkSubTree []     pkg short = []
mkSubTree [s]    pkg short = [Node s True pkg short []]
mkSubTree (s:ss) pkg short = [Node s (null ss) Nothing Nothing (mkSubTree ss pkg short)]

splitModule :: Module -> [String]
splitModule (Module mdl) = split mdl
  where split mdl0 = case break (== '.') mdl0 of
     			(s1, '.':s2) -> s1 : split s2
     			(s1, _)      -> [s1]
