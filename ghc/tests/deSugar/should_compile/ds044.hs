module Test where

-- Strict field unpacking tests: compile with -O -funbox-strict-fields.

-- test 1: simple unboxed int field
data T = T !Int
t (T i) = i + 1

-- test 2: mutual recursion (should back off from unboxing either field)
data R = R !R
data S = S !S

r (R s) = s

-- test 3: multi-level unboxing
data A = A Int !B Int
data B = B !Int

f = A 1 (B 2) 1
g (A x (B y) z) = A x (B (y+2)) z
h (A x (B y) z) = y + 2

-- test 4: flattening nested tuples
data C = C !(Int,Int)
j (C (a,b)) = a + b

-- test 5: polymorphism, multiple strict fields
data D a b = D Int !(a,b) !(E Int)
data E a = E a
k (D a (b,c) (E d)) = a + b + c + d
