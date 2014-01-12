-- !!! cannot close an explicit layout context with a parse error
module ShouldFail where
f = let { x = 42 in x
