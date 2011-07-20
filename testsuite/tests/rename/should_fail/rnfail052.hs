-- Error messages when you use 'forall' *without* the RankN flags
-- Test cases similar to Trac #2114

module ShouldFail where

f :: forall a. a->a
f = error "ur"

g :: Int -> (forall a. a-> a) -> Int
g = error "ur"

data S = MkS (forall a. a->a)
  -- This one complains about 'a' and 'forall' not in scope
  -- because they aren't implicitly quantified, 
  -- whereas implicit quantification deals with the first two
