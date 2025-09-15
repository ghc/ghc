-- Test for #2414
-- Should provoke an occurs-check error

module ShouldFail where

unfoldr :: (b -> Maybe (Bool, b)) -> Bool
unfoldr = unfoldr

f = unfoldr Just
