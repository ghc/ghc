module ShouldFail where
-- !!! Testing bogus (or existential) type variables
type T a = Either a b
