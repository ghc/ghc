module Main where

main :: IO ()
main = do
    let x = -1e3 :: Rational
    print $ case x of
        1e3  -> "1"
        -1e3 -> "-1"
        _  -> "other"

