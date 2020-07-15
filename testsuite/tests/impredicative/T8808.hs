{-# LANGUAGE ImpredicativeTypes, NoMonomorphismRestriction #-}

module Test where

f1 :: Maybe (forall a. [a] -> [a]) -> Maybe ([Int], [Char])
f1 (Just g) = Just (g [3], g "hello")
f1 Nothing  = Nothing

f2 :: [forall a. [a] -> [a]] -> Maybe ([Int], [Char])
f2 [g] = Just (g [3], g "hello")
f2 []  = Nothing

g1  = (f1 . Just) reverse

g1' = f1 (Just reverse)

g2   = f2 [reverse]

-- Left sections not working yet
-- g2'  = f2 ((: []) reverse)

g2'' = f2 (reverse : [])
