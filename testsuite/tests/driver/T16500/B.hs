module B where

import A (message)

main :: IO ()
main = do
    putStrLn message
