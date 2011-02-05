
-- Trac #2538
module ShouldFail where
import Data.Ix

f :: (Eq a => a -> a) -> Int
f = error "urk"

g :: [Eq a => a -> a] -> Int
g = error "urk"

h :: Ix (Eq a => a -> a) => Int
h = error "urk"
