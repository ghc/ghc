-- monobinds
-----

a = 1:a

-----

a = 2

b = 1:a:c

c = 0:b

-----

a = 1

b = a+a

-----

f = \ y -> y
 
g x = x

-----

f True = 1

f False = 0

-----

f (x:y) = x

f z = z

-----

f (True,x) = x

f (False,y) = y+1

-----

