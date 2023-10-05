{-# LANGUAGE UnboxedTuples #-}

module Main where

type Null = (# #)

{-# NOINLINE showNull #-}
showNull :: Null -> String
showNull (# #) = "(# #)"

{-# NOINLINE showNullPair #-}
showNullPair :: (# Null, Null #) -> String
showNullPair (# n1, n2 #) = "(# " ++ showNull n1 ++ ", " ++ showNull n2 ++ " #)"

main :: IO ()
main = do
    putStrLn (showNullPair (# (# #), (# #) #))
