module Algo.HybCC (hybcc) where

import Data.Vector.Unboxed as V

hybcc :: (Int, Vector Int, Vector Int) -> Vector Int
{-# NOINLINE hybcc #-}
hybcc (n, e1, e2) = concomp (V.zip e1 e2) n
    where
      concomp es n
        | V.null es = V.enumFromTo 0 (n-1)
        | otherwise = V.backpermute ins ins
        where
          p = shortcut_all
            $ V.update (V.enumFromTo 0 (n-1)) es

          (es',i) = compress p es
          r = concomp es' (V.length i)
          ins = V.update_ p i
              $ V.backpermute i r

      enumerate bs = V.prescanl' (+) 0 $ V.map (\b -> if b then 1 else 0) bs

      pack_index bs = V.map fst
                    . V.filter snd
                    $ V.zip (V.enumFromTo 0 (V.length bs - 1)) bs

      shortcut_all p | p == pp   = pp
                     | otherwise = shortcut_all pp
        where
          pp = V.backpermute p p

      compress p es = (new_es, pack_index roots)
        where
          (e1,e2) = V.unzip es
          es' = V.map (\(x,y) -> if x > y then (y,x) else (x,y))
              . V.filter (\(x,y) -> x /= y)
              $ V.zip (V.backpermute p e1) (V.backpermute p e2)

          roots = V.zipWith (==) p (V.enumFromTo 0 (V.length p - 1))
          labels = enumerate roots
          (e1',e2') = V.unzip es'
          new_es = V.zip (V.backpermute labels e1') (V.backpermute labels e2')
