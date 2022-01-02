import qualified Data.IntSet as IntSet
import Data.Array

newtype Graph = Graph { neighbors :: Array Int [Int] }

readGraph :: [[Int]] -> Graph
readGraph ([n] : edges) = ans  where
  ans = Graph $ accumArray (flip (:)) [] (1, n) edgesTwice
  edgesTwice = concat [[(u, v), (v, u)] | [u, v] <- edges]
readGraph _ = error "readGraph: bad format"

bfs :: Graph -> Int -> Array Int Int
-- returns an array with the distance from each node to the start node
bfs g start = array (bounds (neighbors g)) $ assocList  where
  assocList = _bfs 0 IntSet.empty (IntSet.singleton start)
  _bfs dist visited currs = if  IntSet.null currs
    then  []
    else  map (\x -> (x, dist)) currli ++ _bfs (dist+1) nvisit ncurr  where
      currli = IntSet.toList currs
      nvisit = IntSet.union visited currs
      ncurr = IntSet.difference nbrs nvisit
      nbrs = IntSet.fromList (concatMap (neighbors g !) currli)

solve :: [[Int]] -> [Int]
solve li@(_:edges) = map ((`mod` 2) . (bfs (readGraph li) 1 !) . head) edges
solve _ = error "no input?"

parse :: String -> [[Int]]
parse = map (map read . words) . lines
