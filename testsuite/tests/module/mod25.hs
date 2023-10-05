module ShouldFail where
-- !!! Testing bogus (or existential) type variables
data T a = K a b
