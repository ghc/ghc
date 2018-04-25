-- !!! Duplicate fields in record decls

module OK where

data X = A {a :: Int} | B {a :: Int}

f x = x

-- data Y = V {a :: Int}

-- f y = y
