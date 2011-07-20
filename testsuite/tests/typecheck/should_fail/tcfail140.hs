-- GHC 6.4 gave pretty horrible error messages 
-- for some of these examples
-- c.f. SourceForge [ ghc-Bugs-1231273 ] confusing error

module ShouldFail where

f :: Int -> Int
f x = x

bar = f 3 9

rot xs = 3 `f` 4

bot xs = map (3 `f`) xs

t = ((\Just x -> x) :: Maybe a -> a) (Just 1)

g :: Int -> Int
g x y = True
  


