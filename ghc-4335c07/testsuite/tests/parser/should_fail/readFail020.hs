-- !!! cannot close a layout context with an explicit close brace
module ShouldFail where
f = let x = 42 } in x
