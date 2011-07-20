module ShouldCompile where

-- I bet this test is a mistake!  From the layout it 
-- looks as if 'test' takes three args, the latter two 
-- of higher rank.  But the parens around these args are
-- missing, so it parses as
--    test :: [a] 
--          -> forall a. Ord a 
--          => [b]
--          -> forall c. Num c
--          => [c]
--          -> [a]
--
-- But maybe that what was intended; I'm not sure
-- Anyway it should typecheck!

test :: [a] -- ^ doc1 
        -> forall b. (Ord b) => [b] {-^ doc2 -} 
        -> forall c. (Num c) => [c] -- ^ doc3
        -> [a]
test xs ys zs = xs
