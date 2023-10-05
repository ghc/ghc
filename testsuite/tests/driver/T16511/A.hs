module A (mainA) where

import B

mainA :: IO ()
mainA = do
    putStrLn "Hello"
    putStrLn name
