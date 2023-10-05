module ShouldFail where
-- !!! Testing recursive type synonyms
type T1 = (Int,T2)
type T2 = (Int,T1)
