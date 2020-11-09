module F13 where

f13 :: Int -> Int -> Int -> Int
f13 x y = let f13f = if (x>0) then \z -> z x y
                                else \z -> y
              f13h = let v = f13f (*)
                     in \w -> w + v
          in \u -> f13h u

