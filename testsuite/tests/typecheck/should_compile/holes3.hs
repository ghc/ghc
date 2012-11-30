{-# LANGUAGE TypeHoles #-}

module Main where

main = return ()

f = _

g :: Int -> Char
g x = _

h = _ ++ "a"

z :: [a] -> [a]
z y = const y _
