-- | Exporting records.
module Bug6( A(A), B(B), b, C(C,c1,c2), D(D,d1), E(E) ) where

-- |
-- This record is exported without its field
data A = A { a :: Int }

-- |
-- .. with its field, but the field is named separately in the export list
-- (the field isn't documented separately since it is already documented here)
data B = B { b :: Int }

-- |
-- .. with fields names as subordinate names in the export
data C = C { c1 :: Int, c2 :: Int }

-- |
-- .. with only some of the fields exported (we can't handle this one -
-- how do we render the declaration?)
data D = D { d1 :: Int, d2 :: Int }

-- | a newtype with a field
newtype E = E { e :: Int }
