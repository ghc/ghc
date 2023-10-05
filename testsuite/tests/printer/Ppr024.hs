import Data.List  ()
import Data.List  hiding ()

infixl 1 `f`
-- infixr 2 `\\\`
infix  3 :==>
infix  4 `MkFoo`

data Foo = MkFoo Int | Float :==> Double

x `f` y = x

(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) xs ys =  xs

(\\\) :: ((Eq a)) => [a] -> [a] -> [a]
(\\\) xs ys =  xs

(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) xs ys =  xs

g x = x + if True then 1 else 2
h x = x + 1::Int

{-# SPECIALISe j :: Int -> Int
                  , Integer -> Integer #-}

j n = n + 1

test = let k x y = x+y in 1 `k` 2 `k` 3

data Rec = (:<-:) { a :: Int, b :: Float }

ng1 x y = negate y

instance (Num a, Num b) => Num (a,b)
  where
   {-# Specialise instance Num (Int,Int) #-}
   negate (a,b) = (ng 'c' a, ng1 'c' b)   where  ng x y = negate y



class Foo1 a where

class Foz a

x = 2 where
y = 3

instance Foo1 Int where

ff = ff where g = g where
type T = Int
