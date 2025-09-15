{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Function
import GHC.Exts

main :: IO ()
main = do
  putStrLn \cases | 1 < 1 -> "foo"
                  | otherwise -> "bar"
  print $ (\cases 1 _ -> error "no"; x y -> x + y) 4 5
  (Just 4) & ("23" & \cases
    "23" Nothing -> print "failed"
    s (Just x) -> putStrLn $ s ++ show x)

  unboxed 1 2# (# 3, "4"# #)

unboxed :: Int -> Int# -> (# Int, Addr# #) -> IO ()
unboxed = \cases 1      1# (# 3     , s #) -> print ()
                 (I# x) y  (# (I# z), s #) -> putStrLn $ show (I# (x +# y +# z)) ++ unpackCString# s
