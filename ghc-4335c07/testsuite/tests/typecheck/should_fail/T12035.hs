module T12035 where
import T12035a
type T = Bool
y = f True

-- This should error that 'type T = Int' doesn't match 'data T',
-- NOT that f expects argument of type T but got Bool.
--
-- NB: This test will start passing if we allow abstract data
-- types to be implemented using type synonyms.
