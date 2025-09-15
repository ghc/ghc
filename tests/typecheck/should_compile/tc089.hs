-- !!! Stress test for type checker

module ShouldSucceed where

import Prelude hiding (head)

one :: a
one = one

head (x:xs) = x

bottom xs = head xs

absIf a b c = a

absAnd a b = head [a,b]

fac_rec fac0 n a 
  = (absIf (absAnd (s_3_0 n) one) 
           (s_2_0 a) 
           (fac0 (absAnd (s_3_2 n) one) (absAnd (s_3_1 n) (s_2_1 a))))

f_rec f0 a 
  = (f0 (s_1_0 a))

g_rec g0 g1 x y z p 
  = (absIf (absAnd (s_3_0 p) one) 
           (absAnd (s_1_0 x) (s_3_0 z)) 
           (absAnd 
              (g0 (s_1_0 y) one one (absAnd (s_3_1 p) one)) 
              (g1 (s_3_2 z) (s_3_1 z) one (absAnd (s_3_2 p) one))))

s_2_0 (v0,v1)   = v0
s_2_1 (v0,v1)   = v1
s_1_0 v0  = v0
s_3_0 (v0,v1,v2)   = v0
s_3_1 (v0,v1,v2)   = v1
s_3_2 (v0,v1,v2)   = v2

fac n a = fac_rec fac_rec4 n a

fac_rec4 n a  = (fac_rec fac_rec3 n a)
fac_rec3 n a  = (fac_rec fac_rec2 n a)
fac_rec2 n a  = (fac_rec fac_rec1 n a)
fac_rec1 n a  = (fac_rec fac_rec0 n a)
fac_rec0 n a  = (bottom [n,a])

f a = (f_rec f_rec2 a)

f_rec2 a  = (f_rec f_rec1 a)
f_rec1 a  = (f_rec f_rec0 a)
f_rec0 a  = (bottom [a])

g x y z p = (g_rec g_rec8 g_rec8 x y z p)

{- 
g x y z p = (g_rec g_rec16 g_rec16 x y z p)

g_rec16 x y z p  = (g_rec g_rec15 g_rec15 x y z p)
g_rec15 x y z p  = (g_rec g_rec14 g_rec14 x y z p)
g_rec14 x y z p  = (g_rec g_rec13 g_rec13 x y z p)
g_rec13 x y z p  = (g_rec g_rec12 g_rec12 x y z p)
g_rec12 x y z p  = (g_rec g_rec11 g_rec11 x y z p)
g_rec11 x y z p  = (g_rec g_rec10 g_rec10 x y z p)
g_rec10 x y z p  = (g_rec g_rec9 g_rec9 x y z p)
g_rec9 x y z p  = (g_rec g_rec8 g_rec8 x y z p)
-}

g_rec8 x y z p  = (g_rec g_rec7 g_rec7 x y z p)
g_rec7 x y z p  = (g_rec g_rec6 g_rec6 x y z p)
g_rec6 x y z p  = (g_rec g_rec5 g_rec5 x y z p)
g_rec5 x y z p  = (g_rec g_rec4 g_rec4 x y z p)
g_rec4 x y z p  = (g_rec g_rec3 g_rec3 x y z p)
g_rec3 x y z p  = (g_rec g_rec2 g_rec2 x y z p)
g_rec2 x y z p  = (g_rec g_rec1 g_rec1 x y z p)
g_rec1 x y z p  = (g_rec g_rec0 g_rec0 x y z p)
g_rec0 x y z p  = (bottom [x,y,z,p])
