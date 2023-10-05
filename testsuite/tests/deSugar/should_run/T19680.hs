module Main where

main :: IO ()
main = do
    let x = -1 :: Integer
    print $ case x of
        1  -> "1"
        -1 -> "-1"
        _  -> "other"
