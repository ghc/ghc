module ShouldFail where
import Array

-- !!! inadvertently using => instead of ->

f :: (Array a) => a -> b
f x = x
