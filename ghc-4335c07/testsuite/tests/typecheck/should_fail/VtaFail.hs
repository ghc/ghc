{-# LANGUAGE TypeApplications, RankNTypes, PolyKinds #-}

module VtaFail1 where

pairup_nosig x y = (x, y)

answer_nosig = pairup_nosig @Int @Bool 5 True

addOne :: Num a => a -> a
addOne x = x + 1

answer_constraint_fail = addOne @Bool 5

answer_lambda = (\x -> x) @Int 12

pair :: forall a. a -> forall b. b -> (a, b)
pair = (,)

a = pair 3 @Int @Bool True

data First (a :: * -> *) = F

first :: First a -> Int
first _ = 0

fInt = first @Int F -- should fail

data Proxy (a :: k) = P -- This expands to P (kind variable) (type variable)

foo :: Proxy a -> Int
foo _ = 0

baz = foo @Bool (P :: Proxy Int) -- should fail

data Three (a :: * -> k -> *) = T

too :: Three a -> Int
too _ = 3

threeBad = too @Maybe T
threeWorse = too @( -> ) (T :: Three Either)

plus :: Int -> Int -> Int
plus = (+)

b = plus @Int 5 7
c = plus @Rational 5 10
d = (+) @Int @Int @Int 12 14


e = show @Int @Float (read "5")
f = show (read @Int @Bool @Float "3")

silly :: a -> Bool
silly _ = False

g = silly @Maybe      -- should fail
