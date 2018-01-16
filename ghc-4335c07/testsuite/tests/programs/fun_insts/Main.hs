-- !!! Defines functions as an instance of Num

module Main where

instance (Eq a, Eq b) => Eq (a->b)
 

instance Show (a->b) where
  show f = "<<function>>"

instance (Num a, Num b) => Num (a->b) where
    f + g    = \x -> f x + g x
    negate f = \x -> - (f x)
    f * g    = \x -> f x * g x
    fromInteger n = \x -> fromInteger n

ss :: Float -> Float
cc :: Float -> Float
tt :: Float -> Float

ss = sin * sin
cc = cos * cos
tt = ss + cc
-- sin**2 + cos**2 = 1

main = putStrLn ((show (tt 0.4))++ "  "++(show (tt 1.652)))
