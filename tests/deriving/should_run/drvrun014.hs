-- This one gave the wrong answer with ghci 5.02.3 (and 5.02.2)

module Main where

infixr 3 :*
infixr 2 :+

data RE a = RE a :+ RE a
          | RE a :* RE a
          | Cat  [RE a]
          | Star (RE a)
          | Plus (RE a)
          | Opt  (RE a)
          | Comp (RE a)
          | Empty
          | Str  [a]
            deriving (Show, Eq, Ord)

main = print (Str "ab" == (Str "a" :+ Str "b"))
