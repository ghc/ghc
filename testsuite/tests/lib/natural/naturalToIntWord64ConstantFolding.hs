module Main (main) where

import Data.Int
import Data.Word

main :: IO ()
main = do p "naturalToInt64"      naturalToInt64
          p "naturalToWord64"     naturalToWord64

    where p :: Show a => String -> a -> IO ()
          p str x = putStrLn (str ++ ": " ++ show x)

naturalToInt64 :: Int64
naturalToInt64 = 100002 + fromInteger 100001

naturalToWord64 :: Word64
naturalToWord64 = 100004 + fromInteger 100003