module ShouldFail where

--!!! inadvertently using => instead of ->

f :: (Array a) => a -> b
f x = x
