class  Eqq a  where
    evenN :: a -> Bool
    oddN	:: a -> Bool
    evenN x		=  False
    oddN x		=  True


data N = Z | S N

instance Eqq N 
  where
   evenN Z = True
   evenN (S x) = oddN x
   oddN Z = False
   oddN (S x) = evenN x

choose x = if evenN x then "hello world (evenN)\n" else if oddN x then "hello world (oddN)\n" else "no!\n"

add n m = 
   case n of
       Z -> m  
       S nn -> S (add nn m)

mul n m = 
   case n of
       Z -> Z
       S nn -> add m (mul nn m)

pow n m = 
   case m of
       Z -> S Z
       S mm -> mul n (pow n mm)

n1 = S Z
n2 = add n1 n1
n4 = add n2 n2
n6 = add n2 n4
n8 = add n2 n6
n10 = add n2 n8
n16 = add n6 n10
n18 = add n8 n10
n20 = add n4 n16

bign = pow n2 n16
bign1 = add bign n1

main = putStr (choose bign1)
