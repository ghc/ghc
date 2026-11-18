module T28158 where

-- Here we miss CPR on y
-- It's a bit of an obscure example, but still; see #28158
f :: a -> a -> a -> [Int] -> [Int]
f a b c xs = let y = case xs of
                      [0] -> Just a
                      [1] -> Just b
                      _   -> Just c
             in h y (case y of
                       Just z  -> reverse (reverse (reverse (
                                  reverse (reverse (reverse (
                                  reverse (reverse xs)))))))
                       Nothing -> xs++xs)

h :: Maybe a -> b -> b
h (Just y) e = h Nothing e
h Nothing  e = e
