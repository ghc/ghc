
module TopSort ( topSort ) where

import List ( nub )



data SCC a = NonRec a | Rec [a]

unNonRec (NonRec x) = x
unRec    (Rec xs)   = xs

isRec (Rec _)    = True
isRec (NonRec _) = False


-- Topologically sort a directed graph, given the forward
-- mapping as a [(source, [dest])].  Detect circularities 
-- and return a Left if they are found.
topSort :: Ord a => [(a, [a])] -> Either [a]{-a circular group-}
                                         [a]{-tsorted order-}
topSort fmap
   = let vertices = nub (map fst fmap ++ concatMap snd fmap)
         f_edges  = concat [ [(src,dst) | dst <- dsts] 
                             | (src, dsts) <- fmap ]
         bmap_for d = (d, [ss | (ss,dd) <- f_edges, dd == d])
         bmap = [bmap_for d | d <- vertices]
         fmap_fn = \ v -> doOrDie (lookup v fmap)
         bmap_fn = \ v -> doOrDie (lookup v bmap)

         doOrDie (Just xs) = xs
         doOrDie Nothing = error "TopSort.topSort: vertex lookup failed?!?"

         sccs       = deScc fmap_fn bmap_fn vertices
         classified = map (classify fmap_fn . unSet) sccs
         recs       = filter isRec classified
         nonrecs    = filter (not.isRec) classified
     in
         if   null recs
         then Right (map unNonRec nonrecs)
         else Left (unRec (head recs))


-- Given the forward edge map, decide whether a SCC is circular or
-- not.
classify :: Eq a => (a -> [a]) -> [a] -> SCC a
classify fmap_fn (x:y:rest)
   = Rec (x:y:rest)
classify fmap_fn [x]
   = if x `elem` fmap_fn x then Rec [x] else NonRec x


-- Appallingly inefficient
newtype Set a = Set [a]

utSetUnion (Set xs) (Set ys) = Set (nub (xs ++ ys))
utSetSingleton x             = Set [x]
utSetEmpty                   = Set []
utSetFromList xs             = Set (nub xs)
utSetElementOf x (Set xs)    = x `elem` xs
unSet (Set xs) = xs





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





deScc :: (Ord a) =>
         (a -> [a]) -> -- The "ins"  map
         (a -> [a]) -> -- The "outs" map
         [a]        -> -- The root vertices
         [Set a]       -- The topologically sorted components
deScc ins outs
   = spanning . depthFirst 
     where depthFirst = snd . deDepthFirstSearch outs (utSetEmpty, [])
           spanning   = snd . deSpanningSearch   ins  (utSetEmpty, [])

