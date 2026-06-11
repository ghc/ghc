-- Exercises -dcanonicalize-local-binds on optimized Core:
-- fusionElemMap yields join points (cj) and term binders (cb);
-- poly yields a type binder (ct) and live case binders (cw).
module T27176 (fusionElemMap, poly) where

fusionElemMap :: Int -> [Int] -> Bool
fusionElemMap x ys = x `elem` map (+ 1) ys

poly :: [a] -> Int
poly xs = case xs of
  []      -> 0
  (_ : t) -> 1 + poly t
