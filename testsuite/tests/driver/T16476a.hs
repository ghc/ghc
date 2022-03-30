module Main where

main :: IO ()
main = do
#if defined(TEST)
    putStrLn "hello"
#endif
    putStrLn "world"
