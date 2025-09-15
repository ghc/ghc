module T16569 where

main :: IO ()
main = putStrLn (case (undefined :: Int) of _ -> undefined)
