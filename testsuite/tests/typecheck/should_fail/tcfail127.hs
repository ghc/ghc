module ShouldFail where

foo :: IO (Num a => a -> a)
foo = error "urk"

-- baz :: (Num a => a -> a) -> Int
-- baz = error "urk"
