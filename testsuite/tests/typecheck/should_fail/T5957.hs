module T5957 where

flex :: Int -> Show a => a -> String
flex i a = show a ++ show i
