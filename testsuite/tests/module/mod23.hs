module ShouldFail where
-- !!! Testing duplicate type variables
type T a a = Either a a
