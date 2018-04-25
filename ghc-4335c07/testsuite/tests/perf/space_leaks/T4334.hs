module Main (main) where

import System.Environment (getArgs)

mkText :: Int -> Int -> Char -> String
mkText ll ln c =
    unlines $ [replicate k c | k <- [ll .. ll+ln]]

main :: IO ()
main = do
    args <- getArgs
    let (ll, ln, c) =
          case args of
            (a1:a2:a3:_) -> (read a1, read a2, head a3)
            (a1:a2:_)    -> (read a1, read a2, 'a')
            (a1:_)       -> (read a1, 3, 'b')
            _            -> (100000, 5, 'c')
    mapM_ (print . length) (lines $ mkText ll ln c)
