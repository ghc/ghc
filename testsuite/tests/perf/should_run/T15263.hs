module Main where

import qualified Data.List as L

expensive :: [Word]
expensive = [1 .. 10000]

cheap :: [Word]
cheap = repeat 2

test_zipWith :: IO ()
test_zipWith = do
    let zw3 = sum $ L.zipWith3 (\a b c -> a*b*c) expensive cheap cheap
        zw4 = sum $ L.zipWith4 (\a b c d -> a*b*c*d) expensive cheap cheap cheap
        zw5 = sum $ L.zipWith5 (\a b c d e ->
            a*b*c*d*e) expensive cheap cheap cheap cheap
        zw6 = sum $ L.zipWith6 (\a b c d e f ->
            a*b*c*d*e*f) expensive cheap cheap cheap cheap cheap
        zw7 = sum $ L.zipWith7 (\a b c d e f g ->
            a*b*c*d*e*f*g) expensive cheap cheap cheap cheap cheap cheap

    putStrLn ("zipWith3: " ++ show zw3)
    putStrLn ("zipWith4: " ++ show zw4)
    putStrLn ("zipWith5: " ++ show zw5)
    putStrLn ("zipWith6: " ++ show zw6)
    putStrLn ("zipWith7: " ++ show zw7)

test_zip3 :: IO ()
test_zip3 = do
    let z3 = foldr (\(x,y,z) acc -> x*y*z+acc) 0 (L.zip3 expensive cheap cheap)

    putStrLn ("zip3: " ++ show z3)

main :: IO ()
main = do
    test_zip3
    test_zipWith
