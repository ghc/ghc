{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Magic

main :: IO ()
main = print $ noinline (\x -> sum $ tardisManual [0..x]) 0

tardisManual :: [Int] -> [Int]
tardisManual xs =
  let
    go []     !acc _ = ([], 0)
    go (_:xs) !acc l =
      let (xs', _) = go xs acc l
      in (l:xs', 0)
    (r, l) = go xs True l
  in r
{-# INLINE tardisManual #-}
