module Test.Haddock.Utils where


import Data.Maybe


mlast :: [a] -> Maybe a
mlast = listToMaybe . reverse
