-- !!! tcfail044: duplicated type variable in instance decls
--
module ShouldFail where

instance (Eq a) => Eq (a->a)
instance Show (a->b)

instance (Num a) => Num (a->a) where
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

--main = putStr ((show (tt 0.4))++ "  "++(show (tt 1.652)))
