module TestData.ParenTree where

import qualified Data.Vector.Unboxed as V

parenTree :: Int -> (V.Vector Int, V.Vector Int)
parenTree n = case go ([],[]) 0 (if even n then n else n+1) of
               (ls,rs) -> (V.fromListN (length ls) (reverse ls),
                           V.fromListN (length rs) (reverse rs))
  where
    go (ls,rs) i j = case j-i of
                       0 -> (ls,rs)
                       2 -> (ls',rs')
                       d -> let k = ((d-2) `div` 4) * 2
                            in
                            go (go (ls',rs') (i+1) (i+1+k)) (i+1+k) (j-1)
      where
        ls' = i:ls
        rs' = j-1:rs


