module ShouldFail where

-- !!! inadvertently using -> instead of =>

f :: (RealFrac a) -> a -> a
f x = x
