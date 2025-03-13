module RecompCompleteIndependence where

import RecompCompletePragmaC

-- Does not refer to any pattern synonyms, so changing them should not affect anything
usePatternP :: MyType -> Bool
usePatternP (StringValue {}) = True
usePatternP _ = False