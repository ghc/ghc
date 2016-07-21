{-# LANGUAGE UnboxedTuples #-}

module Main where

{-# NOINLINE f1 #-}
f1 :: (# #) -> (# #) -> String
f1 (# #) (# #) = "o"

{-# NOINLINE f2 #-}
f2 :: (# (# #), (# #) #) -> String
f2 (# (# #), (# #) #) = "k"

main :: IO ()
main = do
    let t = (# (# #), (# #) #)
    case t of
      (# t1, t2 #) -> putStrLn (f1 t1 t2 ++ f2 t)
