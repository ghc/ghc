module ShouldFail where
-- !!! Testing duplicate type variables
data T a a = K a a
