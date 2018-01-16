-- !!! scoping in list comprehensions right way 'round?
-- a bug reported by Jon Hill
--
module ShouldSucceed where

x = [[True]]
xs :: [Bool]
xs = [x | x <- x, x <- x]
