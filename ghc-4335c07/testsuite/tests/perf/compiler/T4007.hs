
module T4007 where

f :: IO ()
f = sequence_ (replicate 10 (putStrLn "yes"))
