
-- ==========================================================--
-- === Dependancy analyser               dependancy.m (1) ===--
-- ==========================================================--

module Dependancy where
import BaseDefs
import Utils

-- ==========================================================--
--
deBindersOf       :: [(a,b)] -> [a]

deBindersOf defns =  [name | (name, rhs) <- defns]


-- ==========================================================--
--
deValuesOf        :: [(a,b)] -> [b]

deValuesOf defns  =  [rhs  | (name, rhs) <- defns]


-- ==========================================================--
--
deFreeVars :: CExpr -> AnnExpr Naam (Set Naam)

deFreeVars (ENum k)      = (utSetEmpty, ANum k)
deFreeVars (EVar v)      = (utSetSingleton v, AVar v)
deFreeVars (EConstr n)   = (utSetEmpty, AConstr n)

deFreeVars (EAp e1 e2)
   = (utSetUnion (deFreeVarsOf e1') (deFreeVarsOf e2'), AAp e1' e2')
     where e1'            = deFreeVars e1
           e2'            = deFreeVars e2

deFreeVars (ECase e alts)
   = (utSetUnion (deFreeVarsOf e') free, ACase e' alts')
     where e'             = deFreeVars e
	   alts'          = [(t, (ns, deFreeVars e)) | (t, (ns, e)) <- alts]
	   free           = utSetUnionList (map f alts')
	   f (t, (ns, e)) = utSetSubtraction (deFreeVarsOf e) (utSetFromList ns)

deFreeVars (ELam args body)
   = (utSetSubtraction (deFreeVarsOf body') (utSetFromList args), ALam args body')
     where body'          = deFreeVars body

deFreeVars (ELet isRec defns body)
   = (utSetUnion defnsFree bodyFree, ALet isRec defns' body')
     where binders        = deBindersOf defns
	   binderSet      = utSetFromList binders
	   values'        = map deFreeVars (deValuesOf defns)
	   defns'         = zip binders values'
	   freeInValues   = utSetUnionList (map deFreeVarsOf values')
	   defnsFree      | isRec      = utSetSubtraction freeInValues binderSet
			  | otherwise  = freeInValues
	   body'          = deFreeVars body
	   bodyFree       = utSetSubtraction (deFreeVarsOf body') binderSet


-- ==========================================================--
--
deFreeVarsOf :: AnnExpr Naam (Set Naam) -> Set Naam

deFreeVarsOf (free_vars, expr) = free_vars



-- ==========================================================--
--
deDepthFirstSearch :: (Ord a) =>
                      (a -> [a])   -> -- The map,
                      (Set a, [a]) -> -- state: visited set,
                                      --      current sequence of vertices
                      [a]          -> -- input vertices sequence
                      (Set a, [a])    -- final state

deDepthFirstSearch
   = foldl . search
     where
     search relation (visited, sequence) vertex
      | utSetElementOf vertex visited   = (visited,          sequence )
      | otherwise                       = (visited', vertex: sequence')
        where
        (visited', sequence')
         = deDepthFirstSearch relation
	 		   (utSetUnion visited (utSetSingleton vertex), sequence)
			   (relation vertex)



-- ==========================================================--
--
deSpanningSearch   :: (Ord a) =>
                      (a -> [a])       -> -- The map
                      (Set a, [Set a]) -> -- Current state: visited set,
                                          --  current sequence of vertice sets
                      [a]              -> -- Input sequence of vertices
                      (Set a, [Set a])    -- Final state

deSpanningSearch
   = foldl . search
     where
     search relation (visited, utSetSequence) vertex
      | utSetElementOf vertex visited   = (visited,          utSetSequence )
      | otherwise = (visited', utSetFromList (vertex: sequence): utSetSequence)
        where
         (visited', sequence)
            = deDepthFirstSearch relation
			  (utSetUnion visited (utSetSingleton vertex), [])
			  (relation vertex)



-- ==========================================================--
--
deScc :: (Ord a) =>
         (a -> [a]) -> -- The "ins"  map
         (a -> [a]) -> -- The "outs" map
         [a]        -> -- The root vertices
         [Set a]       -- The topologically sorted components

deScc ins outs
   = spanning . depthFirst 
     where depthFirst = second . deDepthFirstSearch outs (utSetEmpty, [])
	   spanning   = second . deSpanningSearch   ins  (utSetEmpty, [])



-- ==========================================================--
--
deDependancy :: CExprP Naam -> CExprP Naam

deDependancy = deDepends . deFreeVars



-- ==========================================================--
--
deDepends (free, ANum n)          = ENum n
deDepends (free, AConstr n)       = EConstr n
deDepends (free, AVar v)          = EVar v
deDepends (free, AAp e1 e2)       = EAp (deDepends e1) (deDepends e2)

deDepends (free, ACase body alts) = ECase (deDepends body)
                                        [(t, (ns, deDepends e))
                                           | (t, (ns, e)) <- alts]

deDepends (free, ALam ns body)    = ELam ns (deDepends body)

deDepends (free, ALet isRec defns body)
   = foldr (deElet isRec) (deDepends body) defnGroups
     where
     binders    = deBindersOf defns
     binderSet  | isRec      = utSetFromList binders
		| otherwise  = utSetEmpty
     edges      = [(n, f) | (n, (free, e)) <- defns,
			    f <- utSetToList (utSetIntersection free binderSet)]
     ins  v     = [u | (u,w) <- edges, v==w]
     outs v     = [w | (u,w) <- edges, v==u]
     components = map utSetToList (deScc ins outs binders)
     defnGroups = [[(n, utSureLookup defns "depends`defnGroups" n) 
					 | n <- ns] | ns <- components]



-- ==========================================================--
--
deElet isRec dfs e 
   = if not isRec || nonRec dfs
        then ELet False dfs' e
        else ELet True  dfs' e
     where dfs' = [(n, deDepends e) | (n,e) <- dfs]
           nonRec [(n, (free, e))] = not (utSetElementOf n free)
           nonRec dfs              = False

-- ==========================================================--
-- === End                               dependancy.m (1) ===--
-- ==========================================================--
