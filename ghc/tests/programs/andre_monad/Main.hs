-- Evaluator in a monad: with execution counts
-- Phil Wadler, 11 October 1991

-- Types are optional.  Some must be commented out to
-- work around a bug in Gofer.

-- The count monad

type  M a		=  (a, Int)

unit			:: a -> M a
unit a			=  (a, 0)

bind			:: M a -> (a -> M b) -> M b
m `bind` k		=  case m of 
                             (a,i) -> case k a of 
                                        (b,j) -> (b,i+j)

-- disp			:: Text a => M a -> String
disp (a,i)		=  show a ++ "\nCount: " ++ show i

tick			:: M ()
tick			=  ((), 1)

-- The evaluator
-- Lines with * are only change from evalIdent

data  Op		=  Add | Sub | Mul | Quo
data  Term		=  Con Int | Bin Op Term Term

eval			:: Term -> M Int
eval (Con i)		=  unit i
eval (Bin op u v)	=  eval u     `bind` (\a  ->
			   eval v     `bind` (\b  ->
			   go op a b  `bind` (\c  ->	-- *
			   tick       `bind` (\ () ->	-- *
			   unit c))))			-- *

go			:: Op -> Int -> Int -> M Int
go Add a b		=  unit (a+b)
go Sub a b		=  unit (a-b)
go Mul a b		=  unit (a*b)
go Quo a b		=  unit (a `quot` b) -- WDP: was "div"

test			:: Term -> String
test t			=  disp (eval t)

-- Test data

add, sub, mul, quo	:: Term -> Term -> Term
u `add` v		=  Bin Add u v
u `sub` v		=  Bin Sub u v
u `mul` v		=  Bin Mul u v
u `quo` v		=  Bin Quo u v

term0,term1,term2	:: Term
term0			=  Con 6 `mul` Con 9
term1			=  (Con 4 `mul` Con 13) `add` Con 2
term2			=  (Con 1 `quo` Con 2) `add` Con 2
term3                   =  ((((((((((((((((((((((((((((((((
                           ((((((((((((((((((((((((((((((
                                 Con 7777 `mul` Con  13) `quo` Con  13)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)
                           `mul` Con 755) `quo` Con 755) `mul` Con 333)
                           `quo` Con 755) `mul` Con 755) `mul` Con 333)

sb 0 = term2
sb n = if (n `mod` 2) == 0
       then term2 `add` (sb (n-1))
       else term2 `sub` (sb (n-1))

main = print (show (eval (sb 5000)))
