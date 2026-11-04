module T27892 where

-- CSE must find that f and g have identical right-hand sides. The
-- binding h is between them in the CoreMap, and each body holds a
-- non-recursive let. See #27892.

f, g, h :: Int -> Int -> Int
f x y = case (if x > y then x * 3 else y * 5) of m -> m * m + x * y
h x y = case (if x > y then x * 7 else y * 9) of m -> m * m + x * y
g x y = case (if x > y then x * 3 else y * 5) of m -> m * m + x * y
