
module TopSort ( topSort ) where

import List ( nub )


-- Pretty scummy.  Really it ought to check for circularities and
-- complain if found.
topSort :: Ord a => [(a, [a])] -> [a]
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

         sccs = deScc fmap_fn bmap_fn vertices
         tsorted = concatMap unSet sccs
     in tsorted


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

