--!!! trying to have a polymorphic type sig where inappropriate
--
module Digraph where

data MaybeErr val err = Succeeded val | Failed err deriving ()

type Edge  vertex = (vertex, vertex)
type Cycle vertex = [vertex]

stronglyConnComp :: Eq vertex => [Edge vertex] -> [vertex] -> [[vertex]]

stronglyConnComp es vs
  = snd (span_tree (new_range reversed_edges)
                   ([],[])
                   ( snd (dfs (new_range es) ([],[]) vs) )
        )
 where
   -- *********** the offending type signature **************
   reversed_edges :: Eq v => [Edge v]
   reversed_edges = map swap es

   -- WRONGOLA: swap :: Eq v => Edge v -> Edge v
   swap (x,y) = (y, x)

   -- WRONGOLA?: new_range :: Eq v => [Edge v] -> v -> [v]

   new_range    []       w = []
   new_range ((x,y):xys) w
       = if x==w
         then (y : (new_range xys w))
         else (new_range xys w)

   {- WRONGOLA?:
   span_tree :: Eq v => (v -> [v])
                     -> ([v], [[v]])
                     -> [v]
                     -> ([v], [[v]])
    -}

   span_tree r (vs,ns) []   = (vs,ns)
   span_tree r (vs,ns) (x:xs)
       | x `elem` vs = span_tree r (vs,ns) xs
       | otherwise = span_tree r (vs',(x:ns'):ns) xs
         where
           (vs',ns') = dfs r (x:vs,[]) (r x)

dfs :: Eq v => (v -> [v])
            -> ([v], [v])
            -> [v]
            -> ([v], [v])

dfs r (vs,ns)   []   = (vs,ns)
dfs r (vs,ns) (x:xs) | x `elem` vs = dfs r (vs,ns) xs
                     | otherwise = dfs r (vs',(x:ns')++ns) xs
                                   where
                                     (vs',ns') = dfs r (x:vs,[]) (r x)
