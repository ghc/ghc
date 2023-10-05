import Data.List (iterate')

-- this should evaluate in constant space
main :: IO ()
main = print $ iterate' (+1) 1 !! 100000000
