{-# LANGUAGE UnboxedTuples, UnboxedSums, MagicHash #-}

module Main where

type Null = (# #)

{-# NOINLINE showNull #-}
showNull :: Null -> String
showNull (# #) = "(# #)"

{-# NOINLINE showNullAlt #-}
showNullAlt :: (# Null | Null #) -> String
showNullAlt (# n1 | #) = "(# " ++ showNull n1 ++ " | #)"
showNullAlt (# | n2 #) = "(# | " ++ showNull n2 ++ " #)"

main :: IO ()
main = do
    putStrLn (showNull (# #))
    putStrLn (showNullAlt (# (# #) | #))
    putStrLn (showNullAlt (# | (# #) #))
