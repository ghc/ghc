
data N = Z | S N

choose1 n1 = 
  case n1 of 
       Z -> "even\n"
       S Z -> "odd\n"
       S (S m) -> choose1 m 
choose2 n1 n2 = 
  case n1 of 
       Z -> choose1 n2
       S Z -> "odd\n"
       S (S m) -> choose2 m n2
choose3 n1 n2 n3 = 
  case n1 of 
       Z -> choose2 n2 n3
       S Z -> "odd\n"
       S (S m) -> choose3 m n2 n3

choose4 n1 n2 n3 n4  = 
  case n1 of 
       Z -> choose3 n2 n3 n4
       S Z -> "odd\n"
       S (S m) -> choose4 m n2 n3 n4 

choose5 n1 n2 n3 n4 n5 = 
  case n1 of 
       Z -> choose4 n2 n3 n4 n5
       S Z -> "odd\n"
       S (S m) -> choose5 m n2 n3 n4 n5

add n m = 
   case n of
       Z -> m  
       S nn -> S (add nn m)

n1 = S Z
n2 = add n1 n1
n4 = add n2 n2
n6 = add n2 n4



main = putStr (choose5 n6 n4 n2 n2 n1)

