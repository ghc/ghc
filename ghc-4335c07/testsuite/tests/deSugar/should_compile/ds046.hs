module ShouldCompile where

-- Strict field unpacking tests: compile with -O -funbox-strict-fields.

-- test 1: simple unboxed int field
data T = T !Int
t (T i) = i + 1

-- test 2: mutual recursion (should back off from unboxing either field)
data R = R !S
data S = S !R

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

-- test 6: records
data F a b = F { x :: !Int, y :: !(Float,Float), z :: !(a,b) }
l F{x = a} = a
m (F a b c) = a
n F{z = (a,b)} = a

-- test 7: newtypes
newtype G a b = G (F a b)
data H a b = H !Int !(G a b) !Int
o (H y (G (F{ x=x })) z) = x + z
