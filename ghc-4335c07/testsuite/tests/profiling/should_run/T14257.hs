{-# LANGUAGE ScopedTypeVariables #-}
module Main where

eval :: forall a b. (a -> b -> b) -> b -> [a] -> b
eval f b xs = load xs []
  where
    load :: [a] -> [a] -> b
    load [] stk          = unload b stk
    load (x:xs) stk      = load xs (x : stk)

    unload :: b -> [a] -> b
    unload  v []         = v
    unload  v (x  : stk) = unload ((f $! x) $! v) stk

main :: IO ()
main = print (eval (||) False (True : replicate 10000000 False))
