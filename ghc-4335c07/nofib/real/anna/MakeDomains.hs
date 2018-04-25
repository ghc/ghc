
-- ==========================================================--
-- === Build abstract domains     File: MakeDomains.m (1) ===--
-- ==========================================================--

module MakeDomains where
import BaseDefs
import Utils
import Dependancy

import Data.List(nub) -- 1.3

-- ==========================================================--
--
mdFreeTVarsIn :: TypeDef ->  -- a type definition
                 [Naam]      -- variables free in it

mdFreeTVarsIn (tn, tvl, cal)
   = utSetToList 
       (utSetSubtraction 
          (utSetFromList allVars)
          (utSetFromList (tvl ++ ["int", "bool", "char"])))
     where
        allVars = concat (map f cal)
        f (n, tel) = concat (map allTVs tel)
        allTVs (TDefVar n) = [n]
        allTVs (TDefCons n tel) = n:concat (map allTVs tel)


-- ==========================================================--
--
mdMakeEdges :: [TypeDef] ->    -- all type definitions
               [(Naam, Naam)]  -- all edges resulting (from, to)

mdMakeEdges tdl
   = concat (map mergeFromTo (zip froms tos))
     where
        k13sel (a, b, c) = a
        froms = map k13sel tdl
        tos = map mdFreeTVarsIn tdl
        mergeFromTo (f, tol) = [(f, t) | t <- tol]


-- ==========================================================--
--
mdTypeDependancy :: [TypeDef] ->    -- all type definitions
                    TypeDependancy  -- list of groups & rec flag

mdTypeDependancy tdl
   = map (singleRec.utSetToList) (deScc ins outs roots)
     where
        edges = mdMakeEdges tdl
        ins  v = [u | (u, w) <- edges, v==w]
        outs v = [w | (u, w) <- edges, v==u]
        roots = nub (map f tdl)
                where 
                   f (a, b, c) = a
        singleRec (a:b:abs) = (True, a:b:abs)
        singleRec [a] 
           = (a `elem` (mdFreeTVarsIn (findAIn tdl)), [a])
             where
                findAIn ((tn, tvl, cal):rest) | a==tn      = (tn, tvl, cal)
                                              | otherwise  = findAIn rest


-- ==========================================================--
--
mdIsRecursiveType :: TypeDependancy -> 
                     Naam ->
                     Bool

mdIsRecursiveType typedependancy typeName
   = search typedependancy
     where
        search ((rf, names):rest) 
           | typeName `elem` names   = rf
           | otherwise               = search rest

     
-- ==========================================================--
-- === end                              MakeDomains.m (1) ===--
-- ==========================================================--
