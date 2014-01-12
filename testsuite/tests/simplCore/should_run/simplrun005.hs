module Main where

main = print (fib' 100)
	-- This will time out unless memoing works properly

data Nat = Z | S Nat
           deriving (Show, Eq)

memo f = g
  where
    fz = f Z
    fs = memo (f . S) 
    g  Z    = fz
    g (S n) = fs n
	-- It is a BAD BUG to inline 'fs' inside g
	-- and that happened in 6.4.1, resulting in exponential behaviour

-- memo f = g (f Z) (memo (f . S))
--        = g (f Z) (g (f (S Z)) (memo (f . S . S)))
--        = g (f Z) (g (f (S Z)) (g (f (S (S Z))) (memo (f . S . S . S))))

fib' :: Nat -> Integer
fib'             =  memo fib
  where
  fib Z          =  0
  fib (S Z)      =  1
  fib (S (S n))  =  fib' (S n) + fib' n

instance Num Nat where
  fromInteger 0        =  Z
  fromInteger n        =  S (fromInteger (n - 1))
  Z + n                =  n
  S m + n              =  S (m + n)
  Z * n                =  Z
  S m * n              =  (m * n) + n
  Z - n                =  Z
  S m - Z              =  S m
  S m - S n            =  m - n

instance Enum Nat where
  succ                 =  S
  pred Z               =  Z
  pred (S n)           =  n
  toEnum               =  fromInteger . toInteger
  fromEnum Z           =  0
  fromEnum (S n)       =  fromEnum n + 1

