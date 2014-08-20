-- Tests that `last` successfully fuses.

main :: IO ()
main = print $ last $ filter odd $ [1::Int ..10000000]
