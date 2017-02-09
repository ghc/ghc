-- Test case of known literal with wraparound
test = case 1 :: Int of
         0x10000000000000001 -> "A"
         _  -> "B"
test2 = case 0x10000000000000001 :: Int of
         1 -> "A"
         _  -> "B"
main = putStrLn $ test ++ test2
