module Main (main) where

import Data.Int
import Data.Word

main :: IO ()
main = do p "integerToInt64"      integerToInt64
          p "integerToWord64"     integerToWord64

    where p :: Show a => String -> a -> IO ()
          p str x = putStrLn (str ++ ": " ++ show x)

integerToInt64 :: Int64
integerToInt64 = 100002 + fromInteger 100001

integerToWord64 :: Word64
integerToWord64 = 100004 + fromInteger 100003