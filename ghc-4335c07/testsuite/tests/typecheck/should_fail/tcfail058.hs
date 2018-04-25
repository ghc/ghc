module ShouldFail where
import Data.Array

-- !!! inadvertently using => instead of ->

f :: (Array a) => a -> b
f x = x
