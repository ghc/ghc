module A (caf, mainx, square) where

import B (idd)

caf :: Int
caf = 23423

mainx :: IO ()
mainx = do
    putStrLn $ show (caf + idd)
    putStrLn "Hello"
    putStrLn "World"

square :: IO Int
square = do
    let ss = "I'm a square"
    putStrLn $ ss
    return $ length ss
