-- AndreasK's mutual-recursion example from #27628: 'y' is passed along
-- to the other function of the Rec group, whose specialisation's RULE
-- rewrites the call, so no reboxing.
-- Expect NO -Wspec-constr-reboxing warning.
module T27628d where

foo :: Maybe Int -> Int
foo x = case x of
  Nothing -> 0
  Just 100 -> foo (Just 99)
  Just 0 -> 1
  y -> bar y

bar :: Maybe Int -> Int
bar y = case y of
  Just 0 -> 3
  Just 100 -> bar (Just 99)
  Nothing -> 4
  Just n -> foo (Just (n - 1))
