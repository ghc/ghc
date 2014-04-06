module ShouldFail where

import T8961a

single :: [a] -> Maybe a
single (Single x) = Just x
single _ = Nothing
