module ShouldFail where

import ImpExp_Exp

single :: [a] -> Maybe a
single (Single x) = Just x
single _ = Nothing
