
module Main (main) where

import Data.Int
import Data.Word

around :: Integer -> [Integer]
around i = [i - 2, i - 1, i, i + 1, i + 2]

dump :: Show a => String -> [a] -> IO ()
dump s xs = do putStrLn "===================================="
               putStrLn s
               mapM_ print xs

main :: IO ()
main = do let xs :: [[Integer]]
              xs = [around 0,
                    around (2^30),
                    around (2^31),
                    around (2^32),
                    around (2^33),
                    around (2^34),
                    around (2^62),
                    around (2^63),
                    around (2^64),
                    around (2^65),
                    around (2^66),
                    around (-(2^30)),
                    around (-(2^31)),
                    around (-(2^32)),
                    around (-(2^33)),
                    around (-(2^34)),
                    around (-(2^62)),
                    around (-(2^63)),
                    around (-(2^64)),
                    around (-(2^65)),
                    around (-(2^66))]
              xsInt :: [[Int]]
              xsInt = map (map fromInteger) xs
              xsIntInteger :: [[Integer]]
              xsIntInteger = map (map toInteger) xsInt
              xsInt32 :: [[Int32]]
              xsInt32 = map (map fromInteger) xs
              xsInt32Integer :: [[Integer]]
              xsInt32Integer = map (map toInteger) xsInt32
              xsInt64 :: [[Int64]]
              xsInt64 = map (map fromInteger) xs
              xsInt64Integer :: [[Integer]]
              xsInt64Integer = map (map toInteger) xsInt64
              xsWord :: [[Word]]
              xsWord = map (map fromInteger) xs
              xsWordInteger :: [[Integer]]
              xsWordInteger = map (map toInteger) xsWord
              xsWord32 :: [[Word32]]
              xsWord32 = map (map fromInteger) xs
              xsWord32Integer :: [[Integer]]
              xsWord32Integer = map (map toInteger) xsWord32
              xsWord64 :: [[Word64]]
              xsWord64 = map (map fromInteger) xs
              xsWord64Integer :: [[Integer]]
              xsWord64Integer = map (map toInteger) xsWord64
          dump "xs" xs
          dump "xsInt" xsInt
          dump "xsIntInteger" xsIntInteger
          dump "xsInt32" xsInt32
          dump "xsInt32Integer" xsInt32Integer
          dump "xsInt64" xsInt64
          dump "xsInt64Integer" xsInt64Integer
          dump "xsWord" xsWord
          dump "xsWordInteger" xsWordInteger
          dump "xsWord32" xsWord32
          dump "xsWord32Integer" xsWord32Integer
          dump "xsWord64" xsWord64
          dump "xsWord64Integer" xsWord64Integer

test :: String -> Integer -> Integer -> IO ()
test what want got
 | want == got = return ()
 | otherwise   = print (what, want, got)

