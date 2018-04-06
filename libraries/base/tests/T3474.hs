import Data.List

-- this should evaluate in constant space
main :: IO ()
main = print $ iterate' (+1) 1 !! 100000000
