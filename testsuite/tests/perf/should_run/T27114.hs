{-# OPTIONS_GHC

-dsuppress-uniques

 #-}
module Main where

import System.Environment

foo :: Foldable t => t a -> Int
foo = undefined

{-# RECOMPUTING Nats #-}
data Nats = Nats Int Nats

{-# NOINLINE loop #-}
loop :: Int -> Nats -> (Int -> IO ()) -> IO ()
loop 0 _           _ = return ()
loop n (Nats i is) k = k i >> loop (n - 1) is k

main :: IO ()
main = do
    args <- getArgs
    let count = case args of
                    (a:args) -> read $ filter (/= '_') a
                    _ -> 10_000_000

    let nats n = Nats n (nats (n + 1))
    let ele_action x = if (x `mod` (count `div` 10)) == 0 then print x else seq x (pure ())
    loop count (nats 0) ele_action
    -- With saring for @Nats@ disabled we will not retain the fully materialized nats data structure.
    -- Instead the second loop will recompute it.
    loop count (nats 0) ele_action

